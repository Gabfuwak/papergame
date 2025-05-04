open State
open Types
open Vector
open Character
open Movable
open Controllable
open Collider
open Drawable
open World
module Pos = Position
open Attacks

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

let switch_texture world char state drawable =
  let anim_key = get_animation_key char state in
  let texture =
    match Hashtbl.find_opt world.resources.textures anim_key with
    | Some tex -> 
        (* Create a copy of the animation with reset state *)
        (match tex with
        | Animation anim ->
            Animation {
              frames = anim.frames;
              framerate = anim.framerate;
              reference = anim.reference;
              current_frame = 0;
              accumulated_time = 0.0;
            }
        | _ -> tex)
    | None -> 
        Gfx.debug "Missing texture: %s" anim_key; 
        Hashtbl.find world.resources.textures "missing"
  in
  drawable.texture <- texture

let update_entity_hitboxes world entity character drawable =

  let (drawable_width, drawable_height) =
    match drawable.texture with
    | Image i -> Gfx.surface_size i.surface
    | Animation a -> Gfx.surface_size a.frames.(0)
    | _ -> (0,0)
  in

  let anim_key = get_animation_key character character.current_state in


  let ref_offset = get_reference drawable in

  match Hashtbl.find_opt world.state.collider_store entity with
  | Some collider ->
      (* Get current animation frame *)
      let current_frame = 
        match drawable.texture with
        | Animation anim -> anim.current_frame
        | _ -> 0
      in
      (* Look up hitboxes for this animation and frame *)
      (match Hashtbl.find_opt world.resources.texture_hitboxes anim_key with
      | Some frame_hitboxes -> 
            
          (* Check if we're within bounds *)
          if current_frame < Array.length frame_hitboxes then
            let frame_boxes = frame_hitboxes.(current_frame) in
              
            if Array.length frame_boxes > 0 then (
              (* Create a copy of the hitboxes to avoid modifying the source data *)
              let boxes_copy = Array.map (fun hitbox -> 
                { hitbox with pos = Vector.sub hitbox.pos ref_offset }
              ) frame_boxes in
              
              (* Flip hitboxes if character is facing left *)
              if not character.facing_right then (
                Array.iter (fun hitbox ->
                  hitbox.pos.x <- -. hitbox.pos.x -. hitbox.width;
                ) boxes_copy
              );
              
              collider.boxes <- boxes_copy;
            ) else (
              (* Check if there's an off-by-one issue where frames are indexed from 1 *)
              if (current_frame + 1) < Array.length frame_hitboxes then
                let frame_boxes_alt = frame_hitboxes.(current_frame + 1) in
                if Array.length frame_boxes_alt > 0 then (
                    
                  let boxes_copy = Array.map (fun hitbox -> 
                    { hitbox with pos = Vector.sub hitbox.pos ref_offset }
                  ) frame_boxes_alt in
                  
                  if not character.facing_right then
                    Array.iter (fun hitbox ->
                  hitbox.pos.x <- -. hitbox.pos.x -. hitbox.width;
                    ) boxes_copy;
                  
                  collider.boxes <- boxes_copy;
                )
            )
              
      | None -> 
          (* Default full-size vulnerable hitbox *)
          
          if Array.length collider.boxes = 0 then (
            collider.boxes <- [|{
              Collider.boxtype = "vulnerable";
              Collider.pos = Vector.sub (Vector.create 0.0 0.0) ref_offset;
              Collider.width = float_of_int drawable_width;
              Collider.height = float_of_int drawable_height;
            }|];
          )
    )
  | None -> 
      Gfx.debug "XXX Entity %d has no collider XXX\n" entity

let string_of_action action =
  match action with
  | Left -> "Left"
  | Right -> "Right"
  | Down -> "Down"
  | Up -> "Up"
  | Jump -> "Jump"
  | Attack { attack_type = 0 } -> "attack_forward"
  | Attack {attack_type=1} -> "attack_up"
  | _ -> failwith "unreachable"


let is_action_made world action controllable =
  match Hashtbl.find_opt controllable.controls action with
  | Some key -> 
      is_key_pressed world key
  | None -> 
      false

let is_animation_complete drawable =
  match drawable.texture with
  | Animation anim -> 
      anim.current_frame >= Array.length anim.frames - 1
  | _ -> 
      true

let transition_state world previous_state new_state character drawable =
  character.previous_state <- previous_state; (* not current state in case we do multiple things at the same time *)
  character.current_state <- new_state;
  if previous_state <> new_state then 
    switch_texture world character new_state drawable

let process_run_transition world prev_state character controllable movable drawable =
  let state_changed = 
    if is_action_made world Left controllable then (
      movable.velocity.x <- -. character.stats.running_speed;
      character.facing_right <- false;
      transition_state world prev_state Running character drawable;
      true
    ) else false
  in
  
  let state_changed = 
    if is_action_made world Right controllable then (
      movable.velocity.x <- character.stats.running_speed;
      character.facing_right <- true;
      transition_state world prev_state Running character drawable;
      true
    ) else state_changed
  in
  
  state_changed

let process_idle_transition world prev_state character controllable movable drawable =
  movable.velocity.x <- 0.0;
  transition_state world prev_state Idle character drawable;
  true
    
let process_jump_prep_transition world prev_state character controllable movable drawable =
  if is_action_made world Jump controllable then (
    movable.velocity.x <- movable.velocity.x *. 0.5; (* Optional: reduce horizontal momentum when jumping *)
    transition_state world prev_state JumpPrep character drawable;
    character.time_in_state <- 0.0; (* Reset time for animation timing *)
    true
  ) else 
    false

(* Initial jump - apply upward force once when transitioning from prep *)
let process_jumping_transition world character controllable movable drawable =
  movable.velocity.y <- movable.velocity.y -. character.stats.jump_force;
  transition_state world JumpPrep Jumping character drawable;
  true

let handle_air_control world movable controllable character =
  if is_action_made world Left controllable then (
    movable.velocity.x <- -. character.stats.air_control;
    character.facing_right <- false;
  ) else if is_action_made world Right controllable then (
    movable.velocity.x <- character.stats.air_control;
    character.facing_right <- true;
  )

(* Continue jumping state - check for transition to top, handle horizontal movement *)
let process_jumping_continue world character controllable movable drawable =
  handle_air_control world movable controllable character;
  transition_state world Jumping Jumping character drawable;
  false

let process_jump_top_transition world character controllable movable drawable =
  handle_air_control world movable controllable character;
  if movable.velocity.y > -.5.0 && character.current_state == Jumping then (
    transition_state world Jumping JumpTop character drawable;
    true
  ) else
    false

let process_falling_transition world prev_state character controllable movable drawable =
  (* Check if we're starting to fall *)
  handle_air_control world movable controllable character;
  if movable.velocity.y > 0.05 then (
    transition_state world prev_state Falling character drawable;
    true
  ) else
    false

let process_continue_falling_transition world character controllable movable drawable =
  handle_air_control world movable controllable character;
  (* Check if we're starting to fall *)
    transition_state world Falling Falling character drawable;
    false

let process_jump_recall_transition world prev_state character controllable movable drawable =
  if character.is_grounded then (
    movable.velocity.x <- 0.0;
    transition_state world prev_state JumpRecall character drawable;
    true
  ) else
    false

let process_ground_transition world character controllable movable position drawable =

  if character.char_name == "ink_master" then(
    let x_correction = if character.facing_right then 44.0 else -44.0 in
    position.Pos.pos <- Vector.add (position.Pos.pos) (Vector.create x_correction 12.0);
  );

  (* Check which ground state to return to based on input *)
  let is_moving = is_action_made world Left controllable || is_action_made world Right controllable in
  if is_moving then (
    transition_state world JumpRecall Running character drawable;
    true
  ) else (
    transition_state world JumpRecall Idle character drawable;
    true
  )

let process_attack_transition world prev_state character controllable movable drawable =
  let attack_forward_control = Attack {attack_type = 0} in
  let attack_forward_state = Attacking {attack_type = 0} in
  let attack_up_control = Attack {attack_type = 1} in
  let attack_up_state = Attacking {attack_type = 1} in
  if is_action_made world attack_forward_control controllable then(
    movable.velocity.x <- 0.0;
    character.hit_entities <- []; (* Reset hit entities list *)
    transition_state world prev_state attack_forward_state character drawable;
    true
  )
  else if is_action_made world attack_up_control controllable then (
    movable.velocity.x <- 0.0;
    character.hit_entities <- []; (* Reset hit entities list *)
    transition_state world prev_state attack_up_state character drawable;
    true
  )
  else
    false  

let process_attack_recall_transition world previous_state character drawable =
  (match previous_state with 
  | Attacking {attack_type} ->
    transition_state world previous_state (AttackRecall{attack_type=attack_type}) character drawable
  | _ -> failwith "unreachable: src/ecs/systems/state_machine.ml"
      );
  true

let process_hit_transition world prev_state character movable drawable =
  match character.pending_hit with
  | Some (hit_vector, stun_time) ->
      character.pending_hit <- None;
      
      movable.velocity <- hit_vector;
      
      transition_state world prev_state 
          (Hit { stun_time; remaining_time = stun_time; hit_vector }) 
          character drawable;
      true
  | None ->
      false
    

let process_death_transition world prev_state character movable drawable =
  if character.health_points <= 0.0 && character.current_state <> Death && character.current_state <> Dead then (
    movable.velocity <- Vector.zero;
    transition_state world prev_state Death character drawable;
    true
  ) else
    false

let process_dead_transition world character drawable =
  transition_state world Death Dead character drawable;
  true


let update_character entity character controllable position movable drawable dt world =
  character.time_in_state <- character.time_in_state +. dt;
  
  (*the transition processing functions *)
  (match character.current_state with
  | Idle ->
    let state_changed = process_hit_transition world Idle character movable drawable in
    let state_changed = if not state_changed then process_attack_transition world Idle character controllable movable drawable else state_changed in
    let state_changed = if not state_changed then process_jump_prep_transition world Idle character controllable movable drawable else state_changed in
    let state_changed = if not state_changed then process_run_transition world Idle character controllable movable drawable else state_changed in
    if not state_changed then
        ignore @@ process_idle_transition world Idle character controllable movable drawable

  | Running ->
    let active = process_hit_transition world Running character movable drawable in
    let active = if not active then process_attack_transition world Running character controllable movable drawable else active in
    let active = if not active then process_jump_prep_transition world Running character controllable movable drawable else active in
    let active = if not active then process_run_transition world Running character controllable movable drawable else active in
    if not active then
      ignore @@ process_idle_transition world Running character controllable movable drawable

  | JumpPrep ->
    let active = process_hit_transition world JumpPrep character movable drawable in
    if is_animation_complete drawable && not active then
      ignore @@ process_jumping_transition world character controllable movable drawable
    else
       ignore @@ process_jump_prep_transition world JumpPrep character controllable movable drawable;
      

  | Jumping ->
    let state_changed = process_hit_transition world Jumping character movable drawable in
    let state_changed = if not state_changed then process_jump_top_transition world character controllable movable drawable else state_changed in
    if not state_changed then
      ignore @@ process_jumping_continue world character controllable movable drawable;

  | JumpTop ->
    let state_changed = process_hit_transition world JumpTop character movable drawable in
    let state_changed = if not state_changed then process_jump_recall_transition world JumpTop character controllable movable drawable else state_changed in
    if not state_changed then
      if is_animation_complete drawable then
        ignore @@ process_falling_transition world JumpTop character controllable movable drawable
      else
        ignore @@ process_jump_top_transition world character controllable movable drawable 

  | Falling ->
    let state_changed = process_hit_transition world Falling character movable drawable in
    let state_changed = if not state_changed then process_jump_recall_transition world Falling character controllable movable drawable else state_changed in
    if not state_changed then
      ignore @@ process_continue_falling_transition world character controllable movable drawable

  | JumpRecall ->
    let state_changed = process_hit_transition world JumpRecall character movable drawable in
    let state_changed = if not state_changed then process_jump_prep_transition world JumpRecall character controllable movable drawable else state_changed in
    let state_changed = if not state_changed then process_run_transition world JumpRecall character controllable movable drawable else state_changed in
    if state_changed && character.char_name == "ink_master" then(
      let x_correction = if character.facing_right then 44.0 else -44.0 in
      position.Pos.pos <- Vector.add (position.Pos.pos) (Vector.create x_correction 12.0);
    );
    if not state_changed && is_animation_complete drawable then(
        ignore @@ process_ground_transition world character controllable movable position drawable;
      )
    | Attacking {attack_type} ->
        let curr_state = Attacking{attack_type=attack_type} in
        let state_changed = process_hit_transition world curr_state character movable drawable in
        
        if not state_changed && is_animation_complete drawable then(
          ignore @@ Attacks.execute_attack world entity character position attack_type;
          ignore @@ process_attack_recall_transition world curr_state character drawable
        )

    | AttackRecall {attack_type} ->
        let curr_state = AttackRecall{attack_type=attack_type} in
        let state_changed = process_hit_transition world curr_state character movable drawable in
        let state_changed = if not state_changed then process_attack_transition world curr_state character controllable movable drawable else state_changed in
        let state_changed = if not state_changed then process_jump_prep_transition world curr_state character controllable movable drawable else state_changed in
        let state_changed = if not state_changed then process_run_transition world curr_state character controllable movable drawable else state_changed in
        if not state_changed && is_animation_complete drawable then(
          apply_position_correction character position attack_type;
          ignore @@ process_idle_transition world curr_state character controllable movable drawable
        )
        
    | Hit { stun_time; remaining_time; hit_vector } ->
        let new_remaining = remaining_time -. dt in

        movable.force <- Vector.add movable.force hit_vector;

        if character.health_points <= 0.0 then
          (* If health is zero, transition to Death *)
          transition_state world character.current_state Death character drawable
        else if new_remaining <= 0.0 then
          if character.is_grounded then
            transition_state world character.current_state Idle character drawable
          else
            transition_state world character.current_state Falling character drawable
        else
          transition_state world character.current_state
            (Hit { stun_time; remaining_time = new_remaining; hit_vector })
            character drawable
    | Death ->
      (* Death state just plays the death animation and then transitions to Dead *)
      if is_animation_complete drawable then
        ignore @@ process_dead_transition world character drawable;

    | Dead -> ()
    | _ -> ());

   update_entity_hitboxes world entity character drawable
  

let update world =
  (* Character state *)
  Hashtbl.iter (fun entity character_state ->
    match Hashtbl.find_opt world.state.controllable_store entity,
          Hashtbl.find_opt world.state.movable_store entity,
          Hashtbl.find_opt world.state.position_store entity,
          Hashtbl.find_opt world.state.drawable_store entity
    with
    | Some controllable, Some movable, Some position, Some drawable->
        update_character entity character_state controllable position movable drawable world.dt world;
    | _ -> ()

  ) world.state.character_store;


