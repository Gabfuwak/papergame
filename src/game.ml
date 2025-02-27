open Types
open World  
open Resources

let white = Color (Gfx.color 255 255 255 255)
let black = Color (Gfx.color 0 0 0 255)

let handle_inputs world =
  match Gfx.poll_event () with
  | NoEvent -> ();
  | KeyDown key -> Hashtbl.replace world.keypresses key true;
  | KeyUp key -> Hashtbl.add world.keypresses key false;
  | _ -> ()

let is_key_pressed world key =
  try
    Hashtbl.find world.keypresses key
  with Not_found ->
    false 

(*
todo
let handle_logic cfg =
  if is_key_pressed cfg cfg.key_up then
    cfg.player_pos.y <- cfg.player_pos.y -. 5.0;
  if is_key_pressed cfg cfg.key_down then
    cfg.player_pos.y <- cfg.player_pos.y +. 5.0;
  if is_key_pressed cfg cfg.key_right then
    cfg.player_pos.x <- cfg.player_pos.x +. 5.0;
  if is_key_pressed cfg cfg.key_left then
    cfg.player_pos.x <- cfg.player_pos.x -. 5.0;
 ()
 *)
let draw_rect world texture x y w h =
  match texture with
  | Color c ->
      Gfx.set_color world.ctx c;
      Gfx.fill_rect world.ctx world.window_surface x y w h
  | Image i -> 
      Gfx.blit_scale world.ctx world.window_surface i x y w h

let update world dt =
  let frame_delta = if world.last_frame_time = 0.0 then 0.0 else dt -. world.last_frame_time in
  world.last_frame_time <- dt;
  
  world.time_acc <- world.time_acc +. frame_delta;
  
  handle_inputs world;
  (* TODO handle_logic world; *)
  
  if world.time_acc >= 1000.0 then begin
    world.resources.curr_texture <- (world.resources.curr_texture + 1) mod 3;
    world.time_acc <- world.time_acc -. 1000.0;
    Printf.printf "switching color to %d\n" world.resources.curr_texture;
    Printf.printf "Test: %s\n" world.resources.test;
  end;
  
  let (width, height) = Gfx.get_window_size world.window in
  draw_rect world white 0 0 width height;
  draw_rect world (Hashtbl.find world.resources.textures world.resources.textures_array.(world.resources.curr_texture)) 100 100(*(int_of_float world.player_pos.x) (int_of_float world.player_pos.y)*) 200 200;
  Gfx.commit world.ctx;
  if world.should_stop then
    Some "Program terminated!"
  else
    None









let run keys =
  let window = Gfx.create "game_canvas:800x600:" in
(*
  let context = Gfx.get_context window in
  let surface = Gfx.get_surface window in
  let red = Color (Gfx.color 255 0 0 255) in
  let green = Color (Gfx.color 0 255 0 255) in
  let blue = Color (Gfx.color 0 0 255 255) in
  let cfg = {
    key_left = keys.(0);
    key_right = keys.(1);
    key_up = keys.(2);
    key_down = keys.(3);
    window = window;
    window_surface = surface;
    ctx = context;
    textures = [| red; green; blue |];
    curr_color = 0;
    time_acc = 0.0;
    last_frame_time = 0.0;
    keypresses = Hashtbl.create 10;
    player_pos = {x=100.0; y = 100.0};
    test = "failed...";
    should_stop = false;
    resources = Hashtbl.create 10;
  } in
  *)

  let world = World.create window keys in
   
  let tileset_res = Gfx.load_file "resources/files/tile_set.txt" in
  Gfx.main_loop 
    (fun dt -> Gfx.get_resource_opt tileset_res)
    (fun tileset -> 
      Resources.load_tileset world.resources world.ctx tileset;
      Gfx.main_loop (fun dt -> update world dt) (fun msg -> 
        Printf.printf "Game ended: %s\n" msg
      )
    )
