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

let update world =
  (* Find all controllable entities *)
  Hashtbl.iter (fun entity controllable ->
    match Hashtbl.find_opt world.state.movable_store entity,
          Hashtbl.find_opt world.state.position_store entity with
    | Some movable, Some position ->
        (* Update velocity based on key presses *)
        let velocity = Vector.create 0.0 0.0 in
        
        if is_key_pressed world "up" then
          velocity.y <- velocity.y -. controllable.speed;
        if is_key_pressed world "down" then
          velocity.y <- velocity.y +. controllable.speed;
        if is_key_pressed world "right" then
          velocity.x <- velocity.x +. controllable.speed;
        if is_key_pressed world "left" then
          velocity.x <- velocity.x -. controllable.speed;
        
        movable.velocity <- velocity
    | _ -> ()
  ) world.state.controllable_store

