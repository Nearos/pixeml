open Lwt.Syntax

type task_data = {
  task_id : int;
  task_type_id : int;
  name : string;
  task : TaskManager.task;
}

type task_type_data = {
  task_type_id: int;
  name : string;
  task_type : TaskManager.task_type;
}

module type TaskManagerData = sig
  type t 

  val from_types : (string * TaskManager.task_type) list -> t Lwt.t

  val add_task : t -> string -> int -> TaskManager.task -> unit Lwt.t 
  
  val remove_task : t -> int -> unit Lwt.t
    
  val task_types : t -> task_type_data list Lwt.t

  val tasks : t -> task_data list Lwt.t

  val task_type_by_id : t -> int -> task_type_data Lwt.t 
  
  val task_by_id : t -> int -> task_data Lwt.t
end 

module TaskManagerData : TaskManagerData = struct 
  type task_manager = {
      task_types : (string * TaskManager.task_type) list;
      mutable existing_tasks : (int * string * TaskManager.task) list;
    }
  type t = task_manager

  let from_types types = 
    Lwt.return {
      task_types = types;
      existing_tasks = []
    }

  let rec list_remove_nth n = function 
  | [] -> []
  | x :: xs -> if n = 0 then xs else x :: list_remove_nth (n-1) xs

  let add_task manager name type_id task = 
    manager.existing_tasks <- (type_id, name, task) :: manager.existing_tasks;
    Lwt.return ()
  
  let remove_task manager id = 
    manager.existing_tasks <- list_remove_nth id manager.existing_tasks;
    Lwt.return ()
    
  let task_types manager = manager.task_types |> List.mapi (fun i (n, t) -> {task_type_id = i; name = n; task_type = t})  |> Lwt.return

  let tasks manager = manager.existing_tasks |> List.mapi (fun i (tid, n, t) -> {task_id = i; task_type_id = tid; name = n; task = t}) |> Lwt.return 

  let task_type_by_id manager id = 
    let (name, task_type) = List.nth manager.task_types id in
    Lwt.return {
      task_type_id = id;
      name = name;
      task_type = task_type;
    }

  let task_by_id manager id = 
    let (type_id, name, task) = List.nth manager.existing_tasks id in 
    Lwt.return {
      task_id = id;
      task_type_id = type_id;
      name = name;
      task = task;
    }

end 

let jsonify_list lst = 
  let rec jsonify_list acc = function
    | [] -> acc
    | [item] -> acc ^ item 
    | item :: items -> jsonify_list (acc ^ item ^ ", ") items 
  in jsonify_list "" lst 

let jsonify_settings (settings : TaskManager.task_settings) : string = 
  let jsonify_setting (key, value) = 
    "\"" ^ key ^"\" : \"" ^ value ^ "\""
  in 
  let content = List.map jsonify_setting settings |> jsonify_list in
  "{" ^ content ^ "}"

let stringify_setting_type = 
  let open TaskManager in 
  function
  | String -> "string"
  | Time -> "time"
  | Int -> "int"

let jsonify_settings_template (settings_template : TaskManager.task_settings_template ) : string = 
  let stringified = List.map (fun (a, b) -> (a, stringify_setting_type b)) settings_template in 
  jsonify_settings stringified

let jsonify_task ({task_id; task_type_id; name = task_name; task} : task_data) : string = 
  "{" 
  ^ "\"name\" : \"" ^ task_name
  ^ "\", \"id\" : " ^ string_of_int task_id
  ^ ", \"type_id\" : " ^ string_of_int task_type_id
  ^ ", \"settings\" : " ^ jsonify_settings (TaskManager.settings task)
  ^ "}"

let jsonify_task_type ({task_type_id; name = task_type_name; task_type} : task_type_data) : string = 
  "{" 
  ^ "\"name\" : \"" ^ task_type_name
  ^ "\", \"id\" : " ^ string_of_int task_type_id
  ^ ", \"settings\" : " ^ jsonify_settings_template task_type.settings
  ^ "}"

let list_index_name_to_json (item_jsonifier : 'a -> string ) (list : 'a list) : string = 
  "["
  ^ (list 
    |> List.map item_jsonifier
    |> jsonify_list)
  ^ "]"

type api_new_task_body = {
  task_type_id : int;
  task_name : string;
  settings : (string * string) list ;
} [@@deriving yojson]

type api_modify_task_body = {
  task_id : int ; 
  settings : (string * string) list;
} [@@deriving yojson]

type api_delete_task_body = {
  task_id : int;
} [@@deriving yojson]

let api_calls (scheduler_mvar : Scheduler.message_sender) (task_manager : TaskManagerData.t) = [
  Dream.get "/api/task_types" (fun _ -> 
      let* task_types = TaskManagerData.task_types task_manager in 
      list_index_name_to_json jsonify_task_type task_types |> Dream.json
    );
  Dream.get "/api/tasks" (fun _ -> 
    let* tasks = TaskManagerData.tasks task_manager in
      list_index_name_to_json jsonify_task tasks |> Dream.json
    );
  (*
    Accepts body
    {
      task_type_id : ..., 
      task_name : ...,
      settings: ...
    }
  *)
  Dream.post "/api/new_task" (fun req -> 
    let*
      body = Dream.body req
    in 
    let decoded_body = 
      body 
        |> Yojson.Safe.from_string
        |> api_new_task_body_of_yojson
    in 
    let* task_type_data = TaskManagerData.task_type_by_id task_manager decoded_body.task_type_id in
    let* task = 
      TaskManager.instantiate 
        scheduler_mvar 
        (decoded_body.settings)
        (task_type_data.task_type)
    in 
    let* () = TaskManagerData.add_task task_manager decoded_body.task_name decoded_body.task_type_id task in
    Dream.json "{}");
  (*
    Accepts body
    {
      task_id: ...,
      settings: ...
    }
    *)
  Dream.post "/api/modify_task" (fun req -> 
    let*
      body = Dream.body req
    in 
    let decoded_body = 
      body 
        |> Yojson.Safe.from_string
        |> api_modify_task_body_of_yojson
    in 
    let* {task_type_id; name = task_name; task = old_task; _}= 
      TaskManagerData.task_by_id task_manager decoded_body.task_id 
    in 
    (* delete old task from list*)
    let* () = TaskManagerData.remove_task task_manager decoded_body.task_id in
    (* call TaskManager.update*)
    let* new_task = 
      TaskManager.update 
        scheduler_mvar
        decoded_body.settings
        old_task
    in 
    (* add new task to list*)
    let* () = TaskManagerData.add_task task_manager task_name task_type_id new_task in
    Dream.json "{}");
  (*
    Accepts body
    {
      task_id: ...
    }*)
  Dream.post "/api/delete_task" (fun req -> 
    let*
      body = Dream.body req
    in 
    let decoded_body = 
      body 
        |> Yojson.Safe.from_string
        |> api_delete_task_body_of_yojson
    in 
    let* {task = old_task; _} = TaskManagerData.task_by_id task_manager decoded_body.task_id in 
    (* delete old task from list*)
    let* () = TaskManagerData.remove_task task_manager decoded_body.task_id in 
    (* call TaskManager.delete *)
    let* () = 
      TaskManager.delete scheduler_mvar old_task
    in Dream.json "{}")    
]