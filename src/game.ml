(* Les types des textures qu'on veut dessiner à l'écran *)
type texture =
    Color of Gfx.color
  | Image of Gfx.surface

let white = Color (Gfx.color 255 255 255 255)
let black = Color (Gfx.color 0 0 0 255)

type vec2 = {
  mutable x: float;
  mutable y: float;

}

type config = {
  (* Informations des touches *)
  key_left: string;
  key_up : string;
  key_down : string;
  key_right : string;

  (* Informations de fenêtre *)
  window : Gfx.window;
  window_surface : Gfx.surface;
  ctx : Gfx.context;

  mutable time_acc : float;
  mutable last_frame_time : float;

  (* Consts *)
  (* Note: some are mutable because they are setup while loading resources *)
  mutable textures : texture array;
  mutable resources : (string, texture) Hashtbl.t;

  (* State *)
  mutable curr_color : int;
  keypresses : (string, bool) Hashtbl.t;
  mutable player_pos : vec2;
  mutable test : string;
  mutable should_stop : bool;
}

let handle_inputs cfg =
  match Gfx.poll_event () with
  | NoEvent -> ();
  | KeyDown key -> Hashtbl.replace cfg.keypresses key true;
  | KeyUp key -> Hashtbl.add cfg.keypresses key false;
  | _ -> ()

let is_key_pressed cfg key =
  try
    Hashtbl.find cfg.keypresses key
  with Not_found ->
    false 

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
  
let draw_rect config texture x y w h =
  match texture with
  | Color c ->
      Gfx.set_color config.ctx c;
      Gfx.fill_rect config.ctx config.window_surface x y w h
  | Image i -> 
      Gfx.blit_scale config.ctx config.window_surface i x y w h

let update cfg dt =
  let frame_delta = if cfg.last_frame_time = 0.0 then 0.0 else dt -. cfg.last_frame_time in
  cfg.last_frame_time <- dt;
  
  cfg.time_acc <- cfg.time_acc +. frame_delta;
  
  handle_inputs cfg;
  handle_logic cfg;
  
  if cfg.time_acc >= 1000.0 then begin
    cfg.curr_color <- (cfg.curr_color + 1) mod 3;
    cfg.time_acc <- cfg.time_acc -. 1000.0;
    Printf.printf "switching color to %d\n" cfg.curr_color;
    Printf.printf "Test: %s\n" cfg.test;
  end;
  
  let (width, height) = Gfx.get_window_size cfg.window in
  draw_rect cfg white 0 0 width height;
  draw_rect cfg cfg.textures.(cfg.curr_color) (int_of_float cfg.player_pos.x) (int_of_float cfg.player_pos.y) 200 200;
  Gfx.commit cfg.ctx;
  if cfg.should_stop then
    Some "Program terminated!"
  else
    None

let parse_tileset tileset =
  (* Split the tileset string into lines to get image filenames *)
  let filenames = String.split_on_char '\n' tileset in
  (* Filter out any empty lines *)
  List.filter (fun s -> String.length s > 0) filenames

let process_tileset_resources cfg resources =
  (* Store each image in the hashtable by its filename *)
  List.iter 
        (fun (filename, res) -> 
          let surface = Gfx.get_resource res in
          let texture = Image surface in
          Hashtbl.add cfg.resources filename texture) 
        resources;
  (* Create an array of the loaded textures for cycling display *)
  let texture_array = Array.of_list (
    List.map 
      (fun (_, res) -> Image (Gfx.get_resource res)) 
      resources
  ) in
  
  (* Update the textures array with our loaded images *)
  cfg.textures <- texture_array;
  cfg.test <- "Loaded " ^ (string_of_int (List.length resources)) ^ " images"; 
  ()


let load_tileset cfg tileset =
  let filenames = parse_tileset tileset in
  
  (* Start loading all images at once *)
  let image_resources = List.map 
    (fun filename -> 
      (filename, Gfx.load_image cfg.ctx ("resources/images/" ^ filename))) 
    filenames in
  
  (* Check if all resources are ready *)
  let rec check_all_ready resources =
    List.for_all (fun (_, res) -> Gfx.resource_ready res) resources
  in
  
  (* Set up the main loop to wait until all resources are ready *)
  Gfx.main_loop
    (fun _dt -> 
      if check_all_ready image_resources then
        Some image_resources
      else
        None)
    (fun resources ->
      process_tileset_resources cfg resources;
    )


let run keys =
  let window = Gfx.create "game_canvas:800x600:" in
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
   
  let tileset_res = Gfx.load_file "resources/files/tile_set.txt" in
  Gfx.main_loop 
    (fun dt -> Gfx.get_resource_opt tileset_res)
    (fun tileset -> 
      load_tileset cfg tileset;
      Gfx.main_loop (fun dt -> update cfg dt) (fun msg -> 
        Printf.printf "Game ended: %s\n" msg
      )
    )
