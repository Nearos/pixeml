
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

(** An instantiation of a task type, such as watering the peonies every day at 5pm with 300ml of water *)
type task

val instantiate : Scheduler.message_sender -> (string * string) list  -> task_type -> task Lwt.t

val delete : Scheduler.message_sender -> task -> unit Lwt.t 

val update : Scheduler.message_sender -> (string * string) list ->  task -> task Lwt.t 

val settings : task -> (string * string) list

val parse_settings : task_type ->  (string * string) list -> task_settings

val time_value : task_setting_value -> Scheduler.TimeOfDay.t

val int_value : task_setting_value -> int

val string_value : task_setting_value -> string

val day_schedule_value : task_setting_value -> Scheduler.Event.day_schedule