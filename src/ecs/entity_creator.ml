open Types
open World
open Entity
open Position
open Movable
open Controllable
open Drawable
open Vector

let create_player world x y tex =
  let id = Entity.create () in
  let position = { pos = Vector.create x y } in
  let movable = { velocity = Vector.create 0.0 0.0; force = Vector.create 0.0 0.0 } in
  let controls = Hashtbl.create 10 in
  Hashtbl.add controls "up" Up;
  Hashtbl.add controls "down" Down;
  Hashtbl.add controls "left" Left;
  Hashtbl.add controls "right" Right;
  let controllable = { speed = 1.0; controls = controls} in
  
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

  let drawable = { screen_pos = Vector.create 0.0 0.0; width = 200; height = 200; texture = texture } in

  Hashtbl.add world.state.drawable_store id drawable;
  
  id




(* PONG (tp 2) *)
let create_paddle world is_right =
  let paddle_height = 200 in
  let paddle_width = 20 in
  let paddle_speed = 2.0 in

  let id = Entity.create () in
  let width, height = Gfx.get_context_logical_size world.ctx in
  let width = float_of_int width in
  let height = float_of_int height in

  let pos_x = if is_right then width -. 10.0 -. (float_of_int paddle_width) else 10.0 in
  let pos_y = (height /. 2.0) -. (float_of_int paddle_height) /. 2.0 in

  let position = { pos = Vector.create pos_x pos_y} in

  let movable = { velocity = Vector.create 0.0 0.0; force = Vector.create 0.0 0.0 } in
  let controls = Hashtbl.create 10 in

  if is_right then(
    Hashtbl.add controls "up" Up;
    Hashtbl.add controls "down" Down;
  )
  else(
    Hashtbl.add controls "z" Up;
    Hashtbl.add controls "s" Down;
  );
    
  let controllable = { speed = paddle_speed; controls = controls } in

  let black = Gfx.color 0 0 0 255 in
  
  let texture = Color black in

  let drawable = { screen_pos = Vector.create pos_x pos_y; width = paddle_width; height = paddle_height; texture = texture } in

  Hashtbl.add world.state.drawable_store id drawable;
  Hashtbl.add world.state.position_store id position;
  Hashtbl.add world.state.movable_store id movable;
  Hashtbl.add world.state.controllable_store id controllable;
  
  id


let create_ball world =
  let ball_size = 10 in
  let id = Entity.create() in

  let width, height = Gfx.get_context_logical_size world.ctx in
  
  let pos_x, pos_y = float_of_int (width/2), float_of_int (height/2) in 

  let position = { pos = Vector.create pos_x pos_y} in

  let movable = { velocity = Vector.create 0.0 0.0; force = Vector.create 0.0 0.0 } in
  
  let black = Gfx.color 0 0 0 255 in
  
  let texture = Color black in

  let drawable = { screen_pos = Vector.create pos_x pos_y; width = ball_size; height = ball_size; texture = texture } in

  Hashtbl.add world.state.position_store id position;
  Hashtbl.add world.state.movable_store id movable;
  Hashtbl.add world.state.drawable_store id drawable;



