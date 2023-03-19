open Lwt.Syntax

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

type task_manager = {
    task_types : (string * TaskManager.task_type) list;
    mutable existing_tasks : (string * TaskManager.task) list;
  }

let rec list_remove_nth n = function 
  | [] -> []
  | x :: xs -> if n = 0 then xs else x :: list_remove_nth (n-1) xs

let jsonify_task ((task_id, task_name, task) : int * string * TaskManager.task) : string = 
  "{" 
  ^ "\"name\" : \"" ^ task_name
  ^ "\", \"id\" : " ^ string_of_int task_id
  ^ ", \"settings\" : " ^ jsonify_settings (TaskManager.settings task)
  ^ "}"

let jsonify_task_type ((task_type_id, task_type_name, task_type) : int * string * TaskManager.task_type) : string = 
  "{" 
  ^ "\"name\" : \"" ^ task_type_name
  ^ "\", \"id\" : " ^ string_of_int task_type_id
  ^ ", \"settings\" : " ^ jsonify_settings_template task_type.settings
  ^ "}"

let list_index_name_to_json (item_jsonifier : int * string * 'a -> string ) (list : (string * 'a) list) : string = 
  "["
  ^ (list 
    |> List.mapi (fun i (name, task) -> item_jsonifier (i, name, task)) 
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

let api_calls (scheduler_mvar : Scheduler.message_sender) (task_manager : task_manager) = [
  Dream.get "/api/task_types" (fun _ -> 
      list_index_name_to_json jsonify_task_type task_manager.task_types |> Dream.json
    );
  Dream.get "/api/tasks" (fun _ -> 
      list_index_name_to_json jsonify_task task_manager.existing_tasks |> Dream.json
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
    let* task = 
      TaskManager.instantiate 
        scheduler_mvar 
        (decoded_body.settings)
        (List.nth task_manager.task_types decoded_body.task_type_id |> snd)
    in 
    task_manager.existing_tasks 
      <- (decoded_body.task_name, task) :: task_manager.existing_tasks;
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
    let (task_name, old_task) = 
      List.nth task_manager.existing_tasks decoded_body.task_id in 
    (* delete old task from list*)
    task_manager.existing_tasks <- list_remove_nth decoded_body.task_id task_manager.existing_tasks;
    (* call TaskManager.update*)
    let* new_task = 
      TaskManager.update 
        scheduler_mvar
        decoded_body.settings
        old_task
    in 
    (* add new task to list*)
    task_manager.existing_tasks <- (task_name, new_task) :: task_manager.existing_tasks;
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
    let (_, old_task) = List.nth task_manager.existing_tasks decoded_body.task_id in 
    (* delete old task from list*)
    task_manager.existing_tasks <- 
      list_remove_nth decoded_body.task_id task_manager.existing_tasks;
    (* call TaskManager.delete *)
    let* () = 
      TaskManager.delete scheduler_mvar old_task
    in Dream.json "{}")    
]