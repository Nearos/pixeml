open Lwt.Syntax

open TaskManagerData

let jsonify_list lst = 
  let rec jsonify_list acc = function
    | [] -> acc
    | [item] -> acc ^ item 
    | item :: items -> jsonify_list (acc ^ item ^ ", ") items 
  in jsonify_list "" lst 

let jsonify_settings (settings : (string * string) list) : string = 
  let jsonify_setting (key, value) = 
    "[\"" ^ key ^"\", \"" ^ value ^ "\"]"
  in 
  let content = List.map jsonify_setting settings |> jsonify_list in
  "[" ^ content ^ "]"

let stringify_setting_type (s_type : TaskManager.task_setting_type) = 
  let open TaskManager in 
  match s_type with
  | String -> "string"
  | Time -> "time"
  | Int -> "int"
  | DaySchedule -> "day_schedule"

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

type api_rename_task_body = {
  task_id : int;
  name : string;
} [@@deriving yojson]

type api_delete_task_body = {
  task_id : int;
} [@@deriving yojson]

let api_calls (scheduler_mvar : Scheduler.message_sender) (module TaskManagerData : S) = 
  let task_manager = TaskManagerData.initial in 
  [
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
    (* print_endline (jsonify_settings decoded_body.settings); *)
    (* add new task to list*)
    let* () = TaskManagerData.add_task task_manager task_name task_type_id new_task in
    Dream.json "{}");
  (*
    {
      task_id : ...,
      name : ...,
    }
     *)
  Dream.post "/api/rename_task" (fun req -> 
      let*
        body = Dream.body req
      in 
      let decoded_body = 
        body 
          |> Yojson.Safe.from_string
          |> api_rename_task_body_of_yojson
      in 
      let* {task_type_id; task; _} = 
        TaskManagerData.task_by_id task_manager decoded_body.task_id 
      in 
      let* () = TaskManagerData.remove_task task_manager decoded_body.task_id in
      let* () = TaskManagerData.add_task task_manager decoded_body.name task_type_id task in
      Dream.json "{}"
      );
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