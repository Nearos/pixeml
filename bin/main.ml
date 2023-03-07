
let () = print_newline ()

let  () = 
    let mvar = Lwt_mvar.create_empty () in
    Lwt_main.run (Lwt.pick [WebServer.server_promise mvar; Scheduler.scheduler mvar])
