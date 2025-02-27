open World
open Position
open Drawable
open Vector
open Types

let white = Color (Gfx.color 255 255 255 255)
let black = Color (Gfx.color 0 0 0 255)

let draw_rect world texture x y w h =
  match texture with
  | Color c ->
      Gfx.set_color world.ctx c;
      Gfx.fill_rect world.ctx world.window_surface x y w h
  | Image i ->
      Gfx.blit_scale world.ctx world.window_surface i x y w h

(* Helper function to draw a single entity *)
let draw_entity world drawable position =
  (* FIXME *)
  drawable.screen_pos <- position.pos;
  let pos_x = int_of_float drawable.screen_pos.x in
  let pos_y = int_of_float drawable.screen_pos.y in
  draw_rect world drawable.tex pos_x pos_y drawable.width drawable.height;
  ()

let update world =
  (* Clear the screen - you might want to use a background color *)
  let (width, height) = Gfx.get_window_size world.window in
  draw_rect world white 0 0 width height;
  
  (* Iterate through all drawable entities *)
  Hashtbl.iter (fun entity drawable ->
    drawable.tex <- Hashtbl.find world.resources.textures "extra_character_a.png";
    (* TODO: make a difference between screen pos and world pos, for example a health bar would not have a Position but it would be drawable with a static screen pos *)
    match Hashtbl.find_opt world.state.position_store entity with
    | Some position ->
        Gfx.debug "Drawing entity number %d\n" entity;
        draw_entity world drawable position
    | None ->
        world.should_stop <- true;
        world.exit_message <- "Entity " ^ string_of_int entity ^ " was drawn without a position";
        ()
  ) world.state.drawable_store;
  
  Gfx.commit world.ctx
