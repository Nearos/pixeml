open Lwt.Syntax 

let server_promise (message_send : Scheduler.message Lwt_mvar.t) = 
    let event_id_generator = 
        let current_event_id = ref 100 in 
        fun () -> 
            current_event_id := !current_event_id + 1;
            !current_event_id
    in
    let* task_manager = 
        TaskManagerData.from_types
                [
                    ("Test Task", TestTaskType.task_t event_id_generator);
                    ("Water Plant", WaterPlantTaskType.task_t event_id_generator);
                ]
    in 
    Dream.serve 
    @@ Dream.logger
    @@ Dream.router (
    TaskManagerApi.api_calls message_send task_manager
    @ [
        Dream.get "/" (fun req -> Dream.redirect req "/index.html");
        Dream.get "/api/add_test_event" (fun _ -> 
            let test_id = event_id_generator () in
            let* () = 
                (* create a test event in 10 seconds *)
                let open Scheduler.TimeOfDay in
                Scheduler.ScheduleTest 
                    (now () + of_hms 0 0 10, test_id)
                |> Lwt_mvar.put message_send 
            in 
            Dream.html ("Created test event with id " ^ string_of_int test_id ^ " 10 seconds from now"));
        
        Dream.get "/api/cancel_event_5" (fun _ -> 
            let* () = 
                (* cancel test event 5 *)
                Scheduler.RemoveEvent 5
                |> Lwt_mvar.put message_send 
            in 
            Dream.html ("Removing event 5"));
        Dream.get "/**" @@ Dream.static "frontend/pixeml-frontend/build/";
    ])