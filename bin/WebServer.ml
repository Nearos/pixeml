open Lwt.Syntax 

let server_promise (message_send : Scheduler.message Lwt_mvar.t) = 
    let test_id = ref 100 in
    Dream.serve 
    @@ Dream.router [
        Dream.get "/api/add_test_event" (fun _ -> 
            let* () = 
                test_id := !test_id + 1;
                (* create a test event in 10 seconds *)
                let open Scheduler.TimeOfDay in
                Scheduler.ScheduleTest 
                    (now () + of_hms 0 0 10, !test_id)
                |> Lwt_mvar.put message_send 
            in 
            Dream.html ("Created test event with id " ^ string_of_int !test_id ^ " 10 seconds from now"))
    ]