open Lwt.Syntax

type task_setting_type 
    = Time
    | Int 
    | String

type task_settings_template = (string * task_setting_type) list
type task_settings = (string * string) list

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

let instantiate (sender : Scheduler.message_sender) (settings : task_settings) (ttype : task_type) : task Lwt.t = 
    let* 
        events = ttype.instantiate sender settings
    in Lwt.return
        {
            settings = settings;
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


let update (sender  : Scheduler.message_sender) (settings : task_settings) (task : task) : task Lwt.t =
    let* () = 
        delete sender task 
    in 
        instantiate sender settings task.typ

let settings (task : task) : task_settings = task.settings