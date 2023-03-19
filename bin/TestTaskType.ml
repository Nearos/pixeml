open Lwt.Syntax


let task_t id_gen = 
  TaskManager.{
  settings = [("Time", TaskManager.Time)];
  instantiate = 
    fun (msend : Scheduler.message_sender) (settings : task_settings) : int list Lwt.t -> 
      let time_parts = 
        settings 
        |> List.assoc "Time"
        |> Str.split (Str.regexp ":")
        |> List.map int_of_string
      in match time_parts with 
      | [h;m;s] -> 
        let time = Scheduler.TimeOfDay.of_hms h m s in 
        let id = id_gen () in
        let* () = Lwt_mvar.put msend (Scheduler.ScheduleTest (time, id)) in 
        Lwt.return [id]
      | _ -> Lwt.return []
        

} 