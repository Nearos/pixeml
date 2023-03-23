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

    let type_structure = task_types[utask.type_id].id === utask.type_id 
        ? task_types[utask.type_id] 
        : linear_search((val) => val.id === utask.type_id, task_types); 

    if(type_structure == null){
        return null;
    }
    
    let result_settings : Array<[string, string, string]> = [];

    let unresolved_setting = false;

    utask.settings.forEach(
        ([setting_name, value], i) => {
            if(type_structure?.settings[i][0] === setting_name){
                result_settings.push([setting_name, type_structure.settings[i][1], value]);
            }else if(type_structure){
                console.log("Doing linear search");
                let searched = linear_search((val) => val[0] === setting_name, type_structure.settings);
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

function InputElement({type, value, onUpdateValue} : {value : string, type : string, onUpdateValue : {(_ : string) : void}}){
    if(type === "time"){
        return (
            <input 
                type="time" 
                onChange={(evt) => { 
                    onUpdateValue(evt.target.value)
                }} 
                value={value} />)
    }
    if(type === "int"){
        return (
            <input 
                type="text" 
                onChange={(evt) => { 
                    onUpdateValue(evt.target.value.split("").filter(c => "1234567890".includes(c)).join(""))
                }} 
                value={value} />)
    }
    return <input type="text" onChange={(evt) => onUpdateValue(evt.target.value)} value={value} />
}

function FormInput( 
    {name, type, value, refreshTrigger, onUpdateValue} 
        : {
            name : string, 
            type : string, 
            value: string, 
            refreshTrigger: boolean,
            onUpdateValue : {(updated : string) : void}}
    )  : JSX.Element {

    let [edited, setEdited] = useState(false);

    useEffect(() => {
        if( value === "") {
            setEdited(true);
        }
    } , [value])

    useEffect(() => {if(value != "") setEdited(false)}, [refreshTrigger])
    
    return (
        <span className="task-individual-setting">
            <label>{name}: </label>
            {
                edited
                    ? //<input type="text" onChange={(evt) => onUpdateValue(evt.target.value)} value={value} />
                        <InputElement value={value} type={type} onUpdateValue={onUpdateValue} />
                    : <span className="task-individual-value" onClick={() => setEdited(true)}>{value}</span>
            }
        </span>
    );
}

function TaskTypeForm(props : {displayError : {(_ :string) : void}, task_type : TaskType, refresh : {() : void}, refreshTrigger : boolean}) : JSX.Element {

    let [settings, setSettings] = useState(
        props.task_type.settings.map(([name, type]) => [name, type, ""]));


    let [name, setName] = useState("");

    useEffect(() => {
        setName("");
        setSettings(
            props.task_type.settings.map(([name, type]) => [name, type, ""]))
    }, [props.refreshTrigger])

    function updateSetting(index: number, value : string){
        let newSettings = [...settings];
        newSettings[index] = [newSettings[index][0], newSettings[index][1], value];
        setSettings(newSettings);
    }

    function addTask(){
        (async () => {
            try{
                let res = await fetch("/api/new_task", {
                    method: "POST",
                    headers : {
                        "Content-Type" : "application/json"
                    },
                    body: JSON.stringify({
                        task_type_id : props.task_type.id,
                        task_name: name,
                        settings: settings.map(([name, type, value])=>[name, type === "time" ? value + ":00" : value])
                    }),
                })
                let parsedRes = await res.json();
           
                props.refresh()
            }catch(_){
                props.displayError("Failed to add task");
            }
        })()
    }

    return (
        <div className="task-list-item task-type-item">
            <div className="task-list-item-title">{props.task_type.name}</div>
            <div className="task-list-item-form">
                <FormInput refreshTrigger={props.refreshTrigger} name="Name" type="string" value={name} onUpdateValue={(val) => setName(val)}/>
                {settings.map( ([name, type, value], index) => {
                        return (
                            <FormInput refreshTrigger={props.refreshTrigger} name={name} type={type} value={value} onUpdateValue={(val) => updateSetting(index, val)}/>
                        )
                    }
                )}
                <button onClick={addTask}>add task</button>
            </div>
        </div>
    );
}

function TaskForm(props : {displayError : {(_ :string) : void}, task : Task, refresh : {() : void}, refreshTrigger : boolean}) : JSX.Element {
    let [settings, setSettings] = useState(props.task.settings);

    let [name, setName] = useState(props.task.name);

    let [nameModified, setNameModified] = useState(false);
    let [settingsModified, setSettingsModified] = useState(false);

    useEffect(() => {
        setSettings(props.task.settings);
        setName(props.task.name);
        setNameModified(false);
        setSettingsModified(false);
    }, [props.task])

    function updateSetting(index: number, value : string){
        let newSettings = [...settings];
        newSettings[index] = [newSettings[index][0], newSettings[index][1], value];
        setSettings(newSettings);
        setSettingsModified(true);
    }
    
    function updateName(newName : string){
        setName(newName);
        setNameModified(true);
    }

    function modifyTask(){
        if(settingsModified)
        (async () => {
            try{
                let res = await fetch("/api/modify_task", {
                    method: "POST",
                    headers : {
                        "Content-Type" : "application/json"
                    },
                    body: JSON.stringify({
                        task_id : props.task.id,
                        settings: settings.map(([name, type, value])=>[name, type === "time" && value.split(":").length == 2 ? value + ":00" : value])
                    }),
                })
                let parsedRes = await res.json();
                props.refresh()
            }catch(e){
                props.displayError("Failed to modify task");
            }
        })();

        if(nameModified)
        (async () => {
            try{
                let res = await fetch("/api/rename_task", {
                    method: "POST",
                    headers : {
                        "Content-Type" : "application/json"
                    },
                    body: JSON.stringify({
                        task_id : props.task.id,
                        name : name
                    }),
                })
                let parsedRes = await res.json();
            
                props.refresh()
            }catch(e){
                props.displayError("Failed to modify task");
            }
        })();
    }

    function deleteTask(){
        (async () => {
            let res = await fetch("/api/delete_task", {
                method: "POST",
                headers : {
                    "Content-Type" : "application/json"
                },
                body: JSON.stringify({
                    task_id : props.task.id,
                }),
            })
            let parsedRes = await res.json();
            if(typeof parsedRes === 'object' && Object.keys(parsedRes).length == 0){
                props.refresh()
            }else{
                props.displayError("Failed to delete task");
            }
        })()
    }


    return (
        <div className="task-list-item task-item">
            <div className="task-list-item-title">{props.task.name}</div>
            <div className="task-list-item-subtitle">{props.task.type_name}</div>
            <div className="task-list-item-form">
                <FormInput refreshTrigger={props.refreshTrigger} name="Name" type="string" value={name} onUpdateValue={updateName}/>
                {settings.map( ([name, type, value], index) => {
                        return (
                            <FormInput refreshTrigger={props.refreshTrigger} name={name} type={type} value={value} onUpdateValue={(val) => updateSetting(index, val)}/>
                        )
                    }
                )}
                <button onClick={modifyTask}>modify task</button>
                <button onClick={deleteTask}> delete task</button>
            </div>
        </div>
    )
}

function TaskManagerList() : JSX.Element {
    let [task_types, set_task_types] = useState<Array<TaskType>>([]); 

    let [tasks, set_tasks] = useState<Array<Task>>([]);

    let [errorMessage, displayError] = useState("");

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

    return (
        <div>
            {
                errorMessage === "" 
                    ? <></> 
                    : <div className="error-display">
                        {errorMessage}
                        <button onClick={() => { displayError("")}}>close</button>
                    </div>
            }
            <div>
                {task_types.map((tt) => <TaskTypeForm displayError={displayError} refreshTrigger={_update_trigger} refresh={trigger_update} task_type={tt}/>)}
                {tasks.map((ts) => <TaskForm displayError={displayError} refreshTrigger={_update_trigger} refresh={trigger_update} task={ts}/>)}
            </div>
        </div>);
}

export default TaskManagerList;