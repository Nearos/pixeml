
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

module type S = sig
  type t 

  val from_types : (string * TaskManager.task_type) list -> t Lwt.t

  val add_task : t -> string -> int -> TaskManager.task -> unit Lwt.t 
  
  val remove_task : t -> int -> unit Lwt.t
    
  val task_types : t -> task_type_data list Lwt.t

  val tasks : t -> task_data list Lwt.t

  val task_type_by_id : t -> int -> task_type_data Lwt.t 
  
  val task_by_id : t -> int -> task_data Lwt.t
end 

module TaskManagerData : S = struct 
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