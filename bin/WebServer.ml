open Lwt.Syntax 

module _ = TaskManager

let server_promise (message_send : Scheduler.message Lwt_mvar.t) = 
    let event_id_generator = 
        let current_event_id = ref 100 in 
        fun () -> 
            current_event_id := !current_event_id + 1;
            !current_event_id
    in
    Dream.serve 
    @@ Dream.router [
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
        (* Insert routes for the task manager api here; they will be declared in another file*)
    ]