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

module EventQueue = Heap.Make(Event)

type message
    = Next
    | ScheduleTest of (TimeOfDay.t * int)
    | ScheduleEvent of Event.t

let rec perform (queue : EventQueue.t) = 
    let open TimeOfDay in
    match EventQueue.top queue with 
    | None -> of_hms 24 0 0 - now () |> to_seconds |> float_of_int
    | Some {time ; action; _} -> 
        let seconds_until  = to_seconds (time - now ()) in 
        if seconds_until < 1 
        then (
            let _ = EventQueue.pop queue in (* TODO: store value for tomorrow *)
            action ();
            perform queue
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
    let queue = EventQueue.empty () in
    
    (** 
        Test events to ensure the scheduler is working properly; 
        these 5 events should be executed in the order of their ids *)
    let _ = List.map 
        (fun (time, id) -> 
            EventQueue.insert (test_event time id) queue)
        (let open TimeOfDay in [
            (now () + of_hms 0 0 20, 3);
            (now () + of_hms 0 0 30, 4);
            (now () + of_hms 0 0 10, 2);
            (now () + of_hms 0 0 5, 1);
            (now () + of_hms 0 2 0, 5)
            ])
    in 
    
    let rec scheduler () =
        let time_until_next = perform queue in 
        let sleep_until_next = let* () = Lwt_unix.sleep time_until_next in Lwt.return Next in
        let* res = Lwt.pick [sleep_until_next; Lwt_mvar.take message_in] in 
        match res with
        | Next -> scheduler ()
        | ScheduleTest (time, id) -> 
            EventQueue.insert 
                (test_event time id)
                queue;
            scheduler ()
        | ScheduleEvent event -> 
            EventQueue.insert event queue;
            scheduler ()
        
    in scheduler ()