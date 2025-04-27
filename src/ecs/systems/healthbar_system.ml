open Types
open World
open Position
open Drawable
open State.Character
open Vector

let health_bar_width = 200.0
let health_bar_height = 20.0
let health_bar_border = 2.0
let health_bar_spacing = 30.0  (* Vertical spacing between multiple health bars *)

let render_health_bar world character index =
  let health_percentage = character.health_points /. character.max_hp in
  let clamped_percentage = max 0.0 (min 1.0 health_percentage) in
  
  let screen_pos = Vector.create 20.0 (20.0 +. (float_of_int index) *. health_bar_spacing) in
  
  Gfx.set_color world.ctx (Gfx.color 100 100 100 255);
  Gfx.fill_rect world.ctx world.window_surface
    (int_of_float screen_pos.x)
    (int_of_float screen_pos.y)
    (int_of_float health_bar_width)
    (int_of_float health_bar_height);
    
  let fill_color =
    if clamped_percentage > 0.6 then
      Gfx.color 50 200 50 255  (* Green for high health *)
    else if clamped_percentage > 0.3 then
      Gfx.color 200 200 50 255  (* Yellow for medium health *)
    else
      Gfx.color 200 50 50 255  (* Red for low health *)
  in
  
  Gfx.debug "Character health: %.1f / %.1f (%.2f%%)\n" 
    character.health_points character.max_hp (clamped_percentage *. 100.0);
    
  let fill_width = int_of_float (clamped_percentage *. (health_bar_width -. 2.0 *. health_bar_border)) in
  Gfx.set_color world.ctx fill_color;
  Gfx.fill_rect world.ctx world.window_surface
    (int_of_float (screen_pos.x +. health_bar_border))
    (int_of_float (screen_pos.y +. health_bar_border))
    fill_width
    (int_of_float (health_bar_height -. 2.0 *. health_bar_border));
  ()
    
let update world =
  (* Render health bars directly in screen space, regardless of camera *)
  let bar_index = ref 0 in
  
  Hashtbl.iter (fun entity character ->
    render_health_bar world character !bar_index;
    incr bar_index
  ) world.state.character_store
