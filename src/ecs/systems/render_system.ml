(* 
   TODO: 
      - Camera placement handling so world pos and screen pos are not the same. 
      - Z index handling
*)

open Types
open Drawable
open World
open Position
module C = Collider

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

let render_entity world entity position drawable =
  update_animation drawable.texture world.dt;
  
  match drawable.texture with
  | Color color ->
      Gfx.set_color world.ctx color;
      Gfx.fill_rect world.ctx world.window_surface 
        (int_of_float position.pos.x) (int_of_float position.pos.y) 
        drawable.width drawable.height
      
  | Image surface ->
      Gfx.blit_scale world.ctx world.window_surface surface
        (int_of_float position.pos.x) (int_of_float position.pos.y)
        drawable.width drawable.height
      
  | Animation anim ->
      let current_frame = anim.frames.(anim.current_frame) in
      Gfx.blit_scale world.ctx world.window_surface current_frame
        (int_of_float position.pos.x) (int_of_float position.pos.y)
        drawable.width drawable.height


(* DEBUG *)
let render_hitbox world position collider =
  let hitbox_color = Gfx.color 255 0 0 128 in  (* Semi-transparent red *)
  Gfx.set_color world.ctx hitbox_color;
  
  Array.iter (fun box ->
    let box_x = position.pos.x +. box.C.pos.x in
    let box_y = position.pos.y +. box.C.pos.y in
    Gfx.fill_rect world.ctx world.window_surface
      (int_of_float box_x) (int_of_float box_y)
      (int_of_float box.C.width) (int_of_float box.C.height)
  ) collider.C.boxes

let update world =
  Gfx.set_color world.ctx (Gfx.color 255 255 255 255);
  let x,y = Gfx.get_context_logical_size world.ctx in
  Gfx.fill_rect world.ctx world.window_surface 0 0 x y;
  
  Hashtbl.iter (fun entity drawable ->
    match Hashtbl.find_opt world.state.position_store entity with
    | Some position -> render_entity world entity position drawable
    | None -> 
        (* TODO: fix this to handle ui elements with no world pos *)
        world.should_stop <- true;
        world.exit_message <- "Error: tried to draw entity " ^ string_of_int entity ^ " but it does not gave a position.\n";
  ) world.state.drawable_store;

  if world.debug_hitboxes then
    Hashtbl.iter (fun entity collider ->
      match Hashtbl.find_opt world.state.position_store entity with
      | Some position -> render_hitbox world position collider
      | None -> ()
    ) world.state.collider_store;
  
  Gfx.commit world.ctx









