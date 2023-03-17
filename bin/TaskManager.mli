
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

(** An instantiation of a task type, such as watering the peonies every day at 5pm with 300ml of water *)
type task

val instantiate : Scheduler.message_sender -> task_settings -> task_type -> task Lwt.t

val delete : Scheduler.message_sender -> task -> unit Lwt.t 

val update : Scheduler.message_sender -> task_settings ->  task -> task Lwt.t 

val settings : task -> task_settings