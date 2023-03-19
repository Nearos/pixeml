open Lwt.Syntax

let task_t id_gen = 
  TaskManager.{
  settings = [("Time", TaskManager.Time)];
  instantiate = 
    fun (msend : Scheduler.message_sender) (settings : task_settings) : int list Lwt.t -> 
      let time_setting = 
        settings 
        |> List.assoc "Time"
      in match Scheduler.TimeOfDay.of_string time_setting with 
      | Some time ->
        let id = id_gen () in
        let* () = Lwt_mvar.put msend (Scheduler.ScheduleTest (time, id)) in 
        Lwt.return [id]
      | None -> Lwt.return []
        

} 