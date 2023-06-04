open Lwt.Syntax

let task_t id_gen =
  TaskManager.{
    settings = [("Time", TaskManager.Time); ("Duration", TaskManager.Int); ("WiringPi Pin", TaskManager.Int); ("Day Schedule", TaskManager.DaySchedule)];
    instantiate = 
      fun (msend : Scheduler.message_sender) (settings : task_settings) : int list Lwt.t -> 
        let time = settings |> List.assoc "Time" |> time_value in 
        let day_schedule = settings |> List.assoc "Day Schedule" |> day_schedule_value in
        let duration = settings |> List.assoc "Duration" |> int_value in 
        let pin = settings |> List.assoc "WiringPi Pin" |> int_value in 
        let id1 = id_gen () in 
        let id2 = id_gen () in 
        let* () = Lwt_mvar.put msend (
          Scheduler.ScheduleEvent Scheduler.Event.{
            action = (fun () -> 
              print_endline ("Opening pin " ^ string_of_int pin));
            id = id1;
            time = time;
            day_schedule;
          })
        in
        let* () = Lwt_mvar.put msend (
          Scheduler.ScheduleEvent Scheduler.Event.{
            action = (fun () -> 
              print_endline ("Closing pin " ^ string_of_int pin));
            id = id2;
            time = 
              (let open Scheduler.TimeOfDay in 
              time + of_seconds duration) ;
            day_schedule;
          })
        in 
        Lwt.return [id1; id2]
  } 