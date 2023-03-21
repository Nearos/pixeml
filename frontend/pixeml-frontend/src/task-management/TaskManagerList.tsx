import React, { useEffect, useState } from "react"
import "./TaskManagerList.css"

interface Identified{
    id : number;
}

interface TaskType {
    name : string;
    id : number;
    settings : Array<[string, string]>;
}

interface Task {
    name : string;
    type_name : string;
    id : number;
    type_id : number;
    settings : Array<[string, string, string]>; //name type value
}

interface UnresolvedTask {
    name : string;
    id : number;
    type_id : number;
    settings : Array<[string, string]>;
}

function resolve_task(utask : UnresolvedTask, task_types : Array<TaskType>) : Task | null {
    function linear_search<Val>(pred : {(arg0: Val): boolean} , list : Array<Val> ) : Val | null{
        let res = null;
        list.forEach((val, _) => {
            if(pred(val)){
                res = val
            }
        })
        return res;
    }

    let type_structure = task_types[utask.type_id].id == utask.type_id 
        ? task_types[utask.type_id] 
        : linear_search((val) => val.id == utask.type_id, task_types); 

    if(type_structure == null){
        return null;
    }
    
    let result_settings : Array<[string, string, string]> = [];

    let unresolved_setting = false;

    utask.settings.forEach(
        ([setting_name, value], i) => {
            if(type_structure?.settings[i][0] == setting_name){
                result_settings.push([setting_name, type_structure.settings[i][1], value]);
            }else if(type_structure){
                console.log("Doing linear search");
                let searched = linear_search((val) => val[0] == setting_name, type_structure.settings);
                if(searched){
                    result_settings.push([setting_name, searched[1], value])
                }else{
                    unresolved_setting = true;
                }
            }
        });

    if(unresolved_setting){
        return null;
    }
    return {
        name : utask.name,
        type_name : type_structure.name,
        id : utask.id,
        type_id : utask.type_id,
        settings : result_settings
    }
}

function TaskTypeForm(props : {task_type : TaskType}) : JSX.Element {
    return (
        <div className="task-list-item task-type-item">
            <div className="task-list-item-title">{props.task_type.name}</div>
        </div>
    );
}

function TaskForm(props : {task : Task}) : JSX.Element {
    return (
        <div className="task-list-item task-item">
            <div className="task-list-item-title">{props.task.name}</div>
            <div className="task-list-item-subtitle">{props.task.type_name}</div>
        </div>
    )
}

function TaskManagerList() : JSX.Element {
    let [task_types, set_task_types] = useState<Array<TaskType>>([]); 

    let [tasks, set_tasks] = useState<Array<Task>>([]);

    let [_update_trigger, _set_update_trigger] = useState(false);

    //to be called when something is put to the server
    //so the list of tasks is reloaded
    let trigger_update = () => {_set_update_trigger(! _update_trigger)};

    useEffect(() => {
        (async () =>{
            let encoded_types = await fetch("/api/task_types");
            let res_types = await encoded_types.json();
            set_task_types(res_types);

            let encoded_tasks = await fetch("/api/tasks");
            let res_tasks = await encoded_tasks.json();
            let resolved_tasks = res_tasks.map((utask : any) => resolve_task(utask, res_types));
            set_tasks(resolved_tasks);
        })()
    }, [_update_trigger])

    return (<div>
        {task_types.map((tt) => <TaskTypeForm task_type={tt}/>)}
        {tasks.map((ts) => <TaskForm task={ts}/>)}
    </div>);
}

export default TaskManagerList;