open TaskManagerData

let query_create_table = 
  unit ->. unit @@
  "
  CREATE TABLE IF NOT EXISTS tasks (id serial primary key, type_id int, name varchar(255);
  CREATE TABLE IF NOT EXISTS task_settings (task_id int, key varchar(255), value varchar(255));"

let query_list_tasks = 
  unit ->* (int, int, string) @@
  "SELECT * FROM tasks"

let query_task_settings = 
  int ->* (string, string) @@
  "SELECT key, value FROM task_settings WHERE task_id = ?"

let query_task_by_id = ()

let query_add_task = ()

module IdMap = Map.Make(Int);

type t = {
  task_types : (string * TaskManager.task_type) array;
  task_running_instances : TaskManager.task IdMap.t;
  database_connection : Caqti_lwt.connection;
}

let from_types (type_list : (string * TaskManager.task_type) list) : t Lwt.t = 
  (* do db connection *)
  let* database_connection = 
    let* connection_result = Caqti_lwt.connect (Uri.of_string "postgresql:///pixeml_tasks") in 
    match connection_result with
    | Ok conn -> Lwt.return conn
    | Error _ -> failWith "Failed to connect to database"
  in 
  let task_running_instances = IdMap.empty in
  (* TODO reinstantiate existing tasks*)
  let _ = () in 
  {
    database_connection;
    task_types = Array.of_list type_list;
    task_running_instances;
  }

val add_task : t -> string -> int -> TaskManager.task -> unit Lwt.t 

val remove_task : t -> int -> unit Lwt.t
  
val task_types : t -> task_type_data list Lwt.t

val tasks : t -> task_data list Lwt.t

val task_type_by_id : t -> int -> task_type_data Lwt.t 

val task_by_id : t -> int -> task_data Lwt.t
