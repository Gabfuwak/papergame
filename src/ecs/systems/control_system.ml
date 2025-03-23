open World
open Position  
open Movable 
open Controllable
open Vector

let is_key_pressed world key =
  let actual_key = 
    match Hashtbl.find_opt world.key_names key with
    | None -> 
        world.should_stop <- true;
        world.exit_message <- "[control_system.ml:is_key_pressed] Error: Key " ^ key ^ " does not exist. ";
        ""
    | Some k -> k
  in
  try
    Hashtbl.find world.keypresses actual_key
  with Not_found ->
    false 


let get_direction control =
  match control with
  | Left -> Vector.create (-1.0) 0.0
  | Right -> Vector.create 1.0 0.0
  | Up -> Vector.create 0.0 (-1.0)
  | Down -> Vector.create 0.0 1.0

let update world =
  Hashtbl.iter (fun entity controllable ->
    match Hashtbl.find_opt world.state.movable_store entity,
          Hashtbl.find_opt world.state.position_store entity with
    | Some movable, Some position ->
        let direction = Vector.create 0.0 0.0 in
        
        Hashtbl.iter (fun key control ->
          if is_key_pressed world key then
            let control_dir = get_direction control in
            direction.x <- direction.x +. control_dir.x;
            direction.y <- direction.y +. control_dir.y
        ) controllable.controls;
        
        let normalized_direction = normalize direction in
        movable.velocity <- Vector.scale normalized_direction controllable.speed
    | _ -> ()
  ) world.state.controllable_store
