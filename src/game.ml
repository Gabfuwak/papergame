(* Les types des textures qu'on veut dessiner à l'écran *)
type texture =
    Color of Gfx.color
  | Image of Gfx.surface

let white = Color (Gfx.color 255 255 255 2555)
let black = Color (Gfx.color 0 0 0 2555)


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
}


(* On crée une fenêtre *)

let draw_rect config texture x y w h = 
  match texture with
  | Color c -> 
      Gfx.set_color config.ctx c;
      Gfx.fill_rect config.ctx config.window_surface x y w h
  | Image i -> failwith "todo: implement draw rect with image"

let update cfg dt = 
  let (width, height) = Gfx.get_window_size cfg.window in
  draw_rect cfg white 0 0 width height;
  draw_rect cfg black 100 100 200 200;
  Gfx.commit cfg.ctx

let run keys = 
  let window = Gfx.create "game_canvas:800x600:" in
  let context = Gfx.get_context window in
  let surface = Gfx.get_surface window in
  let cfg = {
    key_left = keys.(0);
    key_right = keys.(1);
    key_up = keys.(2);
    key_down = keys.(3);
    window = window;
    window_surface = surface;
    ctx = context
  } in

  Gfx.main_loop (fun dt -> update cfg dt; None) (fun () -> ())
