open Lwt.Syntax

module TimeOfDay = struct
    type time_of_day = {
        hour : int;
        minute : int;
        second : int;
    }

    type t = time_of_day

    let of_hms h m s = {hour = h; minute = m; second = s}

    let of_tm (time : Unix.tm) : time_of_day =
        {
            hour = time.tm_hour;
            minute = time.tm_min;
            second = time.tm_sec;
        }

    let to_string {hour; minute; second} = string_of_int hour ^ ":" ^ string_of_int minute ^ ":" ^ string_of_int second

    let to_seconds (time : time_of_day) : int = time.hour * 3600 + time.minute * 60 + time.second

    let of_seconds (seconds : int) : time_of_day =
        let hours = seconds / 3600 in 
        let seconds = seconds mod 3600 in
        let minutes = seconds / 60 in 
        let seconds = seconds mod 60 in 
        {
            hour = hours;
            minute = minutes;
            second = seconds;
        } 

    let compare a b = to_seconds a - to_seconds b

    let now () : t = Unix.gettimeofday () |> Unix.localtime |> of_tm

    let (-) (a : t) (b : t) : t = 
        let seconds_difference = to_seconds a - to_seconds b in 
        of_seconds seconds_difference

    let (+) (a : t) (b : t) : t =
        let seconds_sum = to_seconds a + to_seconds b in
        of_seconds seconds_sum

end

module Event = struct 
    type action = unit -> unit

    type scheduled_event = {
            action : action;
            id : int;
            time : TimeOfDay.t;
        }
    
    type t = scheduled_event

    let compare a b = TimeOfDay.compare a.time b.time

   
end

module SchedulerQueues = struct 
    module EventQueue = Heap.Make(Event)

    type scheduler_queues = {
        events_for_tomorrow : Event.t Batteries.Vect.t ref;
        queue : EventQueue.t
        }
    
    type t = scheduler_queues

    let empty () = {
            events_for_tomorrow = ref Batteries.Vect.empty;
            queue = EventQueue.empty ();
        }

    let insert (event : Event.t) (queues : scheduler_queues) : unit =
        let open TimeOfDay in
        if event.time < now () 
        then queues.events_for_tomorrow := Batteries.Vect.append event !(queues.events_for_tomorrow)
        else EventQueue.insert event queues.queue
    
    let pop (queues : t) = EventQueue.pop queues.queue

    let top (queues : t) = EventQueue.top queues.queue

    let set_for_tomorrow (event : Event.t) (queues : t) =
        queues.events_for_tomorrow := Batteries.Vect.append event !(queues.events_for_tomorrow)

    let restore_for_today (queues : t) =
        let _ = Batteries.Vect.map 
            (fun event -> EventQueue.insert event queues.queue)
            !(queues.events_for_tomorrow)
        in
        queues.events_for_tomorrow := Batteries.Vect.empty

    let remove (pred : Event.t -> bool) (queues : t) = 
        try 
            let index_to_remove = EventQueue.find pred queues.queue in
            EventQueue.remove index_to_remove queues.queue
        with Not_found -> 
            try 
                let index_to_remove = Batteries.Vect.findi pred !(queues.events_for_tomorrow) in 
                queues.events_for_tomorrow := Batteries.Vect.remove index_to_remove 1 !(queues.events_for_tomorrow)
            with Not_found -> ()
end

type message
    = Next
    | ScheduleTest of (TimeOfDay.t * int)
    | ScheduleEvent of Event.t
    | RemoveEvent of int

let rec perform 
        (queues : SchedulerQueues.t) = 
    let open TimeOfDay in
    match SchedulerQueues.top queues with 
    | None -> 
        let seconds_until_midnight =  of_hms 24 0 0 - now () |> to_seconds in 
        let _23_hours_in_seconds = of_hms 23 0 0 |> to_seconds in 
        if seconds_until_midnight > _23_hours_in_seconds 
        then ( (* It't tomorrow. Add back all the events *)
            SchedulerQueues.restore_for_today queues;
            perform queues)
        else (* wait for tomorrow *)
            float_of_int seconds_until_midnight
    | Some ({time ; action; _} as evt) -> 
        let seconds_until  = to_seconds (time - now ()) in 
        if seconds_until < 1 
        then (
            let  _ = SchedulerQueues.pop queues in 
            SchedulerQueues.set_for_tomorrow evt queues;
            action ();
            perform queues
        )
        else 
            float_of_int seconds_until 

(** Generates an event for testing the scheduler. 
    Upon exection of this event, it prints 
    1.  the id 
    2.  scheduled time, and actual time, 
        which should not differ by more than a second *)
let test_event time id = 
    let open Event in {
        action = 
            (fun () -> 
                print_endline 
                    ("action " ^ string_of_int id 
                    ^ " scheduled at " ^ TimeOfDay.to_string time
                    ^ " at: " ^ TimeOfDay.to_string (TimeOfDay.now ())));
        time;
        id;
    }

let scheduler (message_in : message Lwt_mvar.t) = 
    let queues = SchedulerQueues.empty () in
    (*
        Test events to ensure the scheduler is working properly; 
        these 5 events should be executed in the order of their ids *)
    let _ = List.map 
        (fun (time, id) -> 
            SchedulerQueues.insert (test_event time id) queues)
        (let open TimeOfDay in [
            (now () + of_hms 0 0 20, -3);
            (now () + of_hms 0 0 30, -4);
            (now () + of_hms 0 0 10, -2);
            (now () + of_hms 0 0 5, -1);
            (now () + of_hms 0 1 0, 5);
            (of_hms 0 0 10, -101);
            (of_hms 0 0 5, -102);
            (of_hms 12 0 0, -100) (* to ensure there is at least one event after 1 am *)
            ])
    in 
    
    let rec scheduler () =
        let time_until_next = perform queues in 
        let sleep_until_next = 
            let* 
                () = Lwt_unix.sleep time_until_next 
            in Lwt.return Next 
        in
        let* 
            res = Lwt.pick [sleep_until_next; Lwt_mvar.take message_in] 
        in 
        match res with
        | Next -> scheduler ()
        | ScheduleTest (time, id) -> 
            SchedulerQueues.insert 
                (test_event time id)
                queues;
            scheduler ()
        | ScheduleEvent event -> 
            SchedulerQueues.insert event queues;
            scheduler ()
        | RemoveEvent id -> 
            if id >= 0 then
                SchedulerQueues.remove (fun event -> event.id = id) queues;
            scheduler ()
        
    in scheduler ()