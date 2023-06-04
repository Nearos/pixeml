open Lwt.Syntax

type task_setting_type 
    = Time
    | Int 
    | String
    | DaySchedule

type task_setting_value 
    = Time of Scheduler.TimeOfDay.t
    | Int of int 
    | String of string 
    | DaySchedule of Scheduler.Event.day_schedule
    | None

let time_value = function 
    | Time t -> t 
    | _ -> (Scheduler.TimeOfDay.of_hms 0 0 0)

let int_value = function 
    | Int i -> i 
    | _ -> 0 

let string_value = function 
    | String s -> s
    | _ -> ""

let day_schedule_value = function 
    | DaySchedule ds -> ds 
    | _ -> EveryDay
    
type task_settings_template = (string * task_setting_type) list
type task_settings = (string * task_setting_value) list

(** A type of task such as plant watering. 
    This type specifies what type of settings are needed for this task type,
    like quantity of water, as well as including a function that will schedule 
    the events for this task and return the event ids *)
type task_type = {
        settings : task_settings_template;
        instantiate : Scheduler.message_sender -> task_settings -> int list Lwt.t
    }

type task = {
        settings : task_settings;
        events : int list;
        typ : task_type;
    }

let parse_settings (ttype : task_type) (unparsed_settings : (string * string) list) : task_settings = 
    let parse_setting ((s_name, s_type) : string * task_setting_type) : (string * task_setting_value) =
        try 
            let strval = List.assoc s_name unparsed_settings in 
            let parsed = match s_type with 
                | Int -> Int (int_of_string strval)
                | Time -> (match Scheduler.TimeOfDay.of_string strval with
                    | Some a -> Time a
                    | None -> None)
                | String -> String strval 
                | DaySchedule -> DaySchedule (Scheduler.Event.day_schedule_of_string strval)
            in (s_name, parsed)
        with 
        | Not_found -> (s_name, None)
    in
    List.map parse_setting ttype.settings

let instantiate (sender : Scheduler.message_sender) (settings : (string * string) list) (ttype : task_type) : task Lwt.t = 
    let parsed_settings = parse_settings ttype settings in 
    let* 
        events = ttype.instantiate sender parsed_settings
    in Lwt.return
        {
            settings = parsed_settings;
            events = events;
            typ = ttype;
        }

let delete (sender : Scheduler.message_sender) (task : task) : unit Lwt.t =
    let rec delete_events = function  
        | [] -> Lwt.return ()
        | e :: es -> 
            let* () = 
                Lwt_mvar.put sender (Scheduler.RemoveEvent e)
            in delete_events es
    in
   delete_events task.events


let update (sender  : Scheduler.message_sender) (settings : (string * string) list) (task : task) : task Lwt.t =
    let* () = 
        delete sender task 
    in 
        instantiate sender settings task.typ

let settings (task : task) : (string * string) list = 
    let to_string (setting_name, setting_value) =
        let strval = 
            match setting_value with
            | Int v -> string_of_int v 
            | Time tod -> Scheduler.TimeOfDay.to_string tod 
            | String s -> s
            | DaySchedule days -> Scheduler.Event.day_schedule_to_string days
            | None -> ""
        in (setting_name, strval)
    in List.map to_string task.settings 

