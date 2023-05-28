open Lwt.Syntax

let task_t id_gen =
  TaskManager.{
    settings = [("Time", TaskManager.Time); ("Duration", TaskManager.Int); ("Raspberry Pi Pin", TaskManager.Int); ("Day Schedule", TaskManager.DaySchedule)];
    instantiate = 
      fun (msend : Scheduler.message_sender) (settings : task_settings) : int list Lwt.t -> 
        match settings |> List.assoc "Time" |> Scheduler.TimeOfDay.of_string with 
        | None -> Lwt.return []
        | Some time ->
          let day_schedule = settings |> List.assoc "Day Schedule" |> Scheduler.Event.day_schedule_of_string in
          let duration = settings |> List.assoc "Duration" |> int_of_string in 
          let pin = settings |> List.assoc "Raspberry Pi Pin" |> int_of_string in 
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