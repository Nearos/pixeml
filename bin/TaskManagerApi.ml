
let jsonify_settings (settings : TaskManager.task_settings) : string = 
  let jsonify_setting (key, value) = 
    "\"" ^ key ^"\" : \"" ^ value ^ "\""
  in 
  let content = List.map jsonify_setting settings |> List.fold_left (^) "" in
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
    existing_tasks : (string * TaskManager.task) list;
  }

let jsonify_task ((task_id, task_name, task) : int * string * TaskManager.task) : string = 
  "{" 
  ^ "\"name\" : " ^ task_name
  ^ "\"id\" : " ^ string_of_int task_id
  ^ "\"settings\" : " ^ jsonify_settings (TaskManager.settings task)
  ^ "}"

let jsonify_task_type ((task_type_id, task_type_name, task_type) : int * string * TaskManager.task_type) : string = 
  "{" 
  ^ "\"name\" : " ^ task_type_name
  ^ "\"id\" : " ^ string_of_int task_type_id
  ^ "\"settings\" : " ^ jsonify_settings_template task_type.settings
  ^ "}"

let api_calls (task_manager : task_manager ref) = []