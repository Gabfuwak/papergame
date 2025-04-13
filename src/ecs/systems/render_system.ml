(* 
   TODO: 
      - Camera placement handling so world pos and screen pos are not the same. 
      - Z index handling
*)

open Types
open Drawable
open Vector
open World
open Position
module C = Collider

let world_to_screen camera_pos camera screen_width screen_height world_pos =
  let rel_x = world_pos.x -. camera_pos.pos.x in
  let rel_y = world_pos.y -. camera_pos.pos.y in
  
  let scaled_x = rel_x *. camera.Camera.zoom in
  let scaled_y = rel_y *. camera.Camera.zoom in
  
  let screen_x = (screen_width /. 2.0) +. scaled_x in
  let screen_y = (screen_height /. 2.0) +. scaled_y in
  
  Vector.create screen_x screen_y

let screen_to_world camera_pos camera screen_width screen_height screen_pos =
  let rel_x = screen_pos.x -. (screen_width /. 2.0) in
  let rel_y = screen_pos.y -. (screen_height /. 2.0) in
  
  let unscaled_x = rel_x /. camera.Camera.zoom in
  let unscaled_y = rel_y /. camera.Camera.zoom in
  
  let world_x = unscaled_x +. camera_pos.pos.x in
  let world_y = unscaled_y +. camera_pos.pos.y in
  
  Vector.create world_x world_y

let update_animation animation dt =
  match animation with
  | Animation anim ->
      anim.accumulated_time <- anim.accumulated_time +. dt;
      let frame_duration = 1000.0 /. anim.framerate in
      while anim.accumulated_time >= frame_duration do
        anim.accumulated_time <- anim.accumulated_time -. frame_duration;
        anim.current_frame <- (anim.current_frame + 1) mod (Array.length anim.frames)
      done
  | _ -> () (* Not an animation, nothing to update *)


let render_entity world camera camera_pos entity position drawable =
  update_animation drawable.texture world.dt;

  let facing_right = 
    match Hashtbl.find_opt world.state.character_store entity with
    | Some character -> character.facing_right
    | None -> true (* Default facing right if no character component *)
in

   
  let (drawable_width, drawable_height) =
    match drawable.texture with
    | Image i -> Gfx.surface_size i
    | Animation a -> Gfx.surface_size a.frames.(0)
    | _ -> (0,0)
  in

  
  let screen_width, screen_height = Gfx.get_context_logical_size world.ctx in
  let screen_width_f = float_of_int screen_width in
  let screen_height_f = float_of_int screen_height in
  
  let screen_pos = world_to_screen camera_pos camera screen_width_f screen_height_f position.pos in
  
  
  let scaled_width = int_of_float (float_of_int drawable_width *. camera.Camera.zoom) in
  let scaled_height = int_of_float (float_of_int drawable_height *. camera.Camera.zoom) in

  if not facing_right then
    Gfx.set_transform world.ctx 0.0 true false (* Horizontal flip *)
  else
    Gfx.reset_transform world.ctx;
  
  match drawable.texture with
  | Color color ->
      Gfx.set_color world.ctx color;
      Gfx.fill_rect world.ctx world.window_surface
        (int_of_float screen_pos.x) (int_of_float screen_pos.y)
      scaled_width scaled_height;
        
  | Image surface ->
      Gfx.blit_scale world.ctx world.window_surface surface
        (int_of_float screen_pos.x) (int_of_float screen_pos.y)
      scaled_width scaled_height;
        
  | Animation anim ->
      let current_frame = anim.frames.(anim.current_frame) in
      Gfx.blit_scale world.ctx world.window_surface current_frame
        (int_of_float screen_pos.x) (int_of_float screen_pos.y)
      scaled_width scaled_height;

  Gfx.reset_transform world.ctx


(* DEBUG *)
let render_hitbox world camera camera_pos position collider =
  let hitbox_color = Gfx.color 255 0 0 128 in  (* Semi-transparent red *)
  Gfx.set_color world.ctx hitbox_color;
  
  let screen_width, screen_height = Gfx.get_context_logical_size world.ctx in
  let screen_width_f = float_of_int screen_width in
  let screen_height_f = float_of_int screen_height in

  Array.iter (fun box ->
    let box_world_pos = Vector.add position.pos box.C.pos in
    
    let screen_pos = world_to_screen camera_pos camera screen_width_f screen_height_f box_world_pos in
    
    let scaled_width = int_of_float (box.C.width *. camera.Camera.zoom) in
    let scaled_height = int_of_float (box.C.height *. camera.Camera.zoom) in
    
    Gfx.fill_rect world.ctx world.window_surface
      (int_of_float screen_pos.x) (int_of_float screen_pos.y)
      scaled_width scaled_height
  ) collider.C.boxes


let update world =
  let screen_width, screen_height = Gfx.get_context_logical_size world.ctx in
  
  match world.active_camera_id with
  | Some camera_id ->
      (match Hashtbl.find_opt world.state.camera_store camera_id,
             Hashtbl.find_opt world.state.position_store camera_id with
       | Some camera, Some camera_pos ->
           Gfx.set_color world.ctx (Gfx.color 255 255 255 255);
           Gfx.fill_rect world.ctx world.window_surface 0 0 screen_width screen_height;
           
           Hashtbl.iter (fun entity drawable ->
             match Hashtbl.find_opt world.state.position_store entity with
             | Some position -> 
                 render_entity world camera camera_pos entity position drawable
             | None -> ()
           ) world.state.drawable_store;
           
           if world.debug_hitboxes then
             Hashtbl.iter (fun entity collider ->
               match Hashtbl.find_opt world.state.position_store entity with
               | Some position -> 
                   render_hitbox world camera camera_pos position collider
               | None -> ()
             ) world.state.collider_store
       | _, _ -> 
           Gfx.set_color world.ctx (Gfx.color 255 0 255 255); (* Magenta *)
           Gfx.fill_rect world.ctx world.window_surface 0 0 screen_width screen_height
      )
  | None ->
      Gfx.set_color world.ctx (Gfx.color 255 0 255 255); (* Magenta *)
      Gfx.fill_rect world.ctx world.window_surface 0 0 screen_width screen_height;
  
  (* Finalize rendering *)
  Gfx.commit world.ctx









