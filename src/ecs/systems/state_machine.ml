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

let is_action_made world action controllable =
  match Hashtbl.find_opt controllable.controls action with
  | Some key -> is_key_pressed world key 
  | None -> false

let transition_state world previous_state new_state character drawable =
  character.previous_state <- previous_state; (* not current state in case we do multiple things at the same time *)
  character.current_state <- new_state;
  if previous_state <> new_state then 
    switch_texture world new_state drawable

let process_run_transition world prev_state character controllable movable drawable =
  let state_changed = 
    if is_action_made world Left controllable then (
      movable.velocity.x <- movable.velocity.x -. controllable.speed *. world.dt *. 0.001;
      character.facing_right <- false;
      transition_state world prev_state Running character drawable;
      true
    ) else false
  in
  
  let state_changed = 
    if is_action_made world Right controllable then (
      movable.velocity.x <- movable.velocity.x +. controllable.speed *. world.dt *. 0.001;
      character.facing_right <- true;
      transition_state world prev_state Running character drawable;
      true
    ) else state_changed
  in
  
  state_changed

let process_idle_transition world prev_state character controllable movable drawable =
      movable.velocity.x <- 0.0;
      transition_state world prev_state Idle character drawable

let update_character character controllable movable drawable dt world =
  character.time_in_state <- character.time_in_state +. dt;
  
  (*the transition processing functions *)
  match character.current_state with
  | Idle -> 
    let active = process_run_transition world Idle character controllable movable drawable in
    if not active then
        process_idle_transition world Idle character controllable movable drawable

  | Running -> 
    let active = process_run_transition world Running character controllable movable drawable in
    if not active then
        process_idle_transition world Running character controllable movable drawable
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

