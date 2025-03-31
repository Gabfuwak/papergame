open State
open Vector
open Character
open Movable
open Controllable
open Drawable
open World

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

let switch_texture world state drawable = 
  let anim_key = get_animation_key "ink_master" state in
  let texture =
    match Hashtbl.find_opt world.resources.textures anim_key with
    | Some tex -> tex
    | None -> Hashtbl.find world.resources.textures "missing" 
    in
  drawable.texture <- texture

let is_move_made world move controllable =
  match Hashtbl.find_opt controllable.controls move with
  | Some key -> is_key_pressed world key 
  | None -> false


let can_transition from_state to_state =
  match from_state, to_state with
  | Character.Idle, Character.Running -> true
  | Character.Running, Character.Idle -> true
  | Character.Running, Character.Running -> true
  | _ -> false

let transition_state world previous_state new_state character drawable =
  character.previous_state <- character.current_state;
  character.current_state <- new_state;
  if previous_state <> new_state then 
    switch_texture world new_state drawable



let update_character character controllable movable drawable dt world =
  character.time_in_state <- character.time_in_state +. dt;
  
  match character.current_state with
  | Idle -> 
      if is_move_made world Left controllable then (
          movable.velocity.x <- movable.velocity.x -. controllable.speed ;
          character.facing_right <- false;
          transition_state world Idle Running character drawable
      );
      if  is_move_made world Right controllable
        then (
          movable.velocity.x <- movable.velocity.x +. controllable.speed ;
          character.facing_right <- true;
          transition_state world Idle Running character drawable
      );
      
    if not (is_move_made world Left controllable || is_move_made world Right controllable) then
      (
      movable.velocity.x <- 0.0;
      transition_state world Idle Idle character drawable
      )

  | Running -> 
      if is_move_made world Left controllable then (
          movable.velocity.x <- movable.velocity.x -. controllable.speed ;
          character.facing_right <- false;
          transition_state world Running Running character drawable
      );
      if  is_move_made world Right controllable
        then (
          movable.velocity.x <- movable.velocity.x +. controllable.speed ;
          character.facing_right <- true;
          transition_state world Running Running character drawable
      );
      
    if not (is_move_made world Left controllable || is_move_made world Right controllable) then
      (
      movable.velocity.x <- 0.0;
      transition_state world Running Idle character drawable
      )

  | _ -> ()

let update world =
  (* Character state *)
  Hashtbl.iter (fun entity character_state ->
    match Hashtbl.find_opt world.state.controllable_store entity,
          Hashtbl.find_opt world.state.movable_store entity,
          Hashtbl.find_opt world.state.position_store entity,
          Hashtbl.find_opt world.state.drawable_store entity
    with
    | Some controllable, Some movable, Some position, Some drawable->
        update_character character_state controllable movable drawable world.dt world;
    | _ -> ()

  ) world.state.character_store

