
let () = print_newline ()

(* let test_database = 
    let mvar = Lwt_mvar.create_empty () in
    let* _ = TaskManagerDatabase.restore_from_database mvar [] in 
    Lwt.return () *)

let  () = 
    let mvar = Lwt_mvar.create_empty () in
    Lwt_main.run (Lwt.pick [WebServer.server_promise mvar; Scheduler.scheduler mvar])
