open Types
open World
open Entity
open State
open Position
open Movable
open Controllable
open Collider
open Drawable
open Camera
open Vector

let create_player world x y tex =
  let id = Entity.create () in
  let position = { pos = Vector.create x y } in
  let movable = { velocity = Vector.create 0.0 0.0; force = Vector.create 0.0 0.0 } in
  let controls = Hashtbl.create 10 in
  Hashtbl.add controls Up "up";
  Hashtbl.add controls Down "down";
  Hashtbl.add controls Left "left";
  Hashtbl.add controls Right "right";
  Hashtbl.add controls Jump "space";
  let controllable = {controls = controls} in
  
  Hashtbl.add world.state.position_store id position;
  Hashtbl.add world.state.movable_store id movable;
  Hashtbl.add world.state.controllable_store id controllable;

  let black = Gfx.color 0 0 0 255 in
  
  let texture =
    match Hashtbl.find_opt world.resources.textures tex with
    | Some text -> text
    | None -> 
        Gfx.debug "Error: texture %s does not exist\n" tex;
        Color black
  in
  
  let drawable = { texture = texture } in

  Hashtbl.add world.state.drawable_store id drawable;

  let stats = {
    Character.air_control = 200.0;
    Character.running_speed = 200.0;
    Character.jump_force = 800.0;
  } in

  let character = {
    Character.stats = stats;
    Character.current_state = State.Character.Idle;
    Character.previous_state = State.Character.Idle;
    Character.time_in_state = 0.0;
    Character.facing_right = true;
    Character.is_grounded = false;
  } in

  Hashtbl.add world.state.character_store id character;


  let hitboxes =
    match Hashtbl.find_opt world.resources.texture_hitboxes tex with
    | Some text -> text.(0)
    | None -> 
         [|{
            boxtype = "vulnerable";
            pos = Vector.create 0.0 0.0;
            width = 200.0;
            height = 200.0;
         }|]
  in


  let collider = {
    origin_pos = position.pos;
    boxes = hitboxes;
    weight = 100.0;
  } in

  Hashtbl.add world.state.collider_store id collider;

  
  id


let create_camera world target x y width height zoom =
  let id = Entity.create () in
  let position = { pos = Vector.create x y } in
  let camera = { 
    zoom = zoom; 
    width = width; 
    height = height;
    target = target;
    offset = Vector.create 0.0 0.0;
  } in
  
  let movable = { velocity = Vector.create 0.0 0.0; force = Vector.create 0.0 0.0 } in
  
  Hashtbl.add world.state.position_store id position;
  Hashtbl.add world.state.camera_store id camera;
  Hashtbl.add world.state.movable_store id movable;
  
  if world.active_camera_id = None then
    world.active_camera_id <- Some id;
  
  id


let create_wall world x y width height =
  let id = Entity.create() in
  
  let position = { pos = Vector.create x y } in
  let hitbox = {
    boxtype = "";
    pos = Vector.create 0.0 0.0;
    width = width;
    height = height;
  } in
  
  let collider = {
    origin_pos = position.pos;
    boxes = [| hitbox |];
    weight = Float.infinity;
  } in

  let black = Gfx.color 0 0 0 255 in
  let texture = Color black in
  let drawable = {
    texture = texture
  } in
  
  Hashtbl.add world.state.position_store id position;
  Hashtbl.add world.state.collider_store id collider;
  Hashtbl.add world.state.drawable_store id drawable;
  
  id
