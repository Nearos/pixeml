
let () = print_newline ()

(* let test_database = 
    let mvar = Lwt_mvar.create_empty () in
    let* _ = TaskManagerDatabase.restore_from_database mvar [] in 
    Lwt.return () *)

let () = Lwt.async_exception_hook := (function
  | Unix.Unix_error (error, func, arg) ->
    Logs.warn (fun m ->
      m  "Client connection error %s: %s(%S)"
        (Unix.error_message error) func arg
    )
  | exn -> Logs.err (fun m -> m "Unhandled exception: %a" Fmt.exn exn)
)

let  () = 
    let mvar = Lwt_mvar.create_empty () in
    Lwt_main.run (Lwt.pick [WebServer.server_promise mvar; Scheduler.scheduler mvar])
