module type S = TaskManagerData.S

open Caqti_request.Infix
open Caqti_type

open Lwt.Syntax 

let query_create_table1 = 
  unit ->. unit @@
  "CREATE TABLE IF NOT EXISTS tasks (id serial primary key, type_id int, name varchar(255))" 

let query_create_table2 = 
  unit ->. unit @@
  "CREATE TABLE IF NOT EXISTS task_settings (task_id int, key varchar(255), value varchar(255))"

let query_list_tasks = 
  unit ->* tup3 int int string @@
  "SELECT * FROM tasks"

let query_task_settings = 
  int ->* tup2 string string @@
  "SELECT key, value FROM task_settings WHERE task_id = ?"

let query_task_by_id = 
  int ->? tup3 int int string @@
  "SELECT * FROM tasks WHERE id = ?" 

let query_add_task = 
  tup2 int string ->? int @@
  "INSERT INTO task (type_id, name) VALUES (?, ?) RETURNING id"

let query_delete_task = 
  tup2 int int ->. unit @@
  "DELETE FROM tasks WHERE id = ?;
  DELETE FROM task_settings WHERE task_id = ?"

let query_add_setting = 
  tup3 int string string ->. unit @@
  "INSERT INTO task_settings (task_id, key, value) VALUES (?, ?, ?)"

let get_task_settings (module Conn : Caqti_lwt.CONNECTION) ((task_id, type_id, name) : (int * int * string)) =
  let* settings = Conn.collect_list query_task_settings task_id in 
  match settings with 
  | Ok setting_list -> Lwt.return (task_id, type_id, name, setting_list)
  | Error _ -> failwith "failed to get task settings"

let get_task_by_id (module Conn : Caqti_lwt.CONNECTION) (task_id : int) =
  let* task = Conn.find_opt query_task_by_id task_id in 
  match task with 
  | Ok (Some task) -> get_task_settings (module Conn) task 
  | _ -> failwith "failed to find that task"

let get_tasks (module Conn : Caqti_lwt.CONNECTION) = 
  let* tasks = Conn.collect_list query_list_tasks () in 
  let rec get_task_list_settings = function
  | [] -> Lwt.return []
  | (task :: rest) -> 
    let* this_task = get_task_settings (module Conn) task in 
    let* rest_task = get_task_list_settings rest in 
    Lwt.return (this_task :: rest_task)
  in match tasks with 
  | Error _ -> failwith "Failed to fetch task list"
  | Ok tasks -> get_task_list_settings tasks

let create_tables (module Conn : Caqti_lwt.CONNECTION) = 
  let* first = Conn.exec query_create_table1 () in 
  match first with 
  | Error e -> Lwt.return (Error e)
  | Ok _ -> Conn.exec query_create_table2 ()

let add_task (module Conn : Caqti_lwt.CONNECTION) (name : string) (type_id : int) (settings : (string * string) list) : int Lwt.t =
  let* res = Conn.find_opt query_add_task (type_id, name) in
  match res with
  | Error _ | Ok (None) -> failwith "failed to add task" 
  | Ok (Some id) -> 
    (let rec add_settings = function
    | [] -> Lwt.return ()
    | (name, value) :: rest -> 
      let* res = Conn.exec query_add_setting (id, name, value) in 
      (match res with
      | Error _ -> failwith "failed to add setting"
      | Ok _ -> add_settings rest)
    in 
    let* () = add_settings settings in 
    Lwt.return id)


let delete_task (module Conn : Caqti_lwt.CONNECTION) (task_id : int) = Conn.exec query_delete_task (task_id, task_id)

module IdMap = Map.Make(Int)

type task_manager_data = {
    task_types : (string * TaskManager.task_type) array;
    task_running_instances : TaskManager.task IdMap.t;
    database_connection : Caqti_lwt.connection;
  }

module Make 
  (Init : sig
    val initial : task_manager_data
  end)
  = struct
  
  type t = task_manager_data

  let initial = Init.initial

  (* val add_task : t -> string -> int -> TaskManager.task -> unit Lwt.t  *)

  (* val remove_task : t -> int -> unit Lwt.t *)
    
  (* val task_types : t -> task_type_data list Lwt.t *)

  (* val tasks : t -> task_data list Lwt.t *)

  (* val task_type_by_id : t -> int -> task_type_data Lwt.t  *)

  (* val task_by_id : t -> int -> task_data Lwt.t *)

end 

module type TT = sig val initial : task_manager_data end

let restore_from_database (msend : Scheduler.message_sender) (type_list : (string * TaskManager.task_type) list) = 
  (* do db connection *)
  let* database_connection = 
    let* connection_result = Caqti_lwt.connect (Uri.of_string "postgresql://pixeml:pixeml_nine_1_seven@localhost:5432/pixeml_tasks") in 
    match connection_result with
    | Ok conn -> Lwt.return conn
    | Error _ -> failwith "Failed to connect to database"
  in 
  let* ctr = create_tables database_connection in
  (match ctr with 
  | Ok _ -> print_endline "created tables"
  | Error err -> failwith @@ Caqti_error.show err);
  let* database_tasks = get_tasks database_connection in 
  let* task_running_list  = 
    Lwt_seq.of_list database_tasks 
      |> Lwt_seq.map_s (fun (id, type_id, _, settings) -> 
        let (_, ttype) = List.nth type_list type_id in 
        let* task = TaskManager.instantiate msend settings ttype in 
        Lwt.return (id, task)
        )
      |> Lwt_seq.to_list
  in let task_running_instances =
      task_running_list 
      |> List.to_seq
      |> IdMap.of_seq
  in
  (* let task_running_instances = IdMap.of_seq running_instances_list in *)
  (* TODO reinstantiate existing tasks*)
  let _ = () in 
  Lwt.return (module Make (struct 
  let initial = {
    database_connection;
    task_types = Array.of_list type_list;
    task_running_instances;
  }
  end) : TT)