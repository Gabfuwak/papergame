include Gfx_base
open Tsdl

let backend = "sdl"

type surface = Sdl.texture

type context = {
  renderer : Sdl.renderer;
  texture : Sdl.texture;
  mutable lwidth : int;
  mutable lheight : int;
  srect : Sdl.rect;
  osrect : Sdl.rect option;
  drect : Sdl.rect;
  mutable angle : float;
  mutable flip : Sdl.flip;
}

type window = {
  window : Sdl.window;
  context : context;
}

type color = int * int * int * int
type font = Tsdl_ttf.Ttf.font
type 'a resource = 'a

let resource_ready _ = true
let get_resource r = r
let get_resource_opt r = Some r
let initialized = ref false

let result = function
  | Error (`Msg s) -> gfx_error "%s" s
  | Ok x -> x

let debug_ri_info ri =
  Format.eprintf "SDL Render driver: %s@\n" ri.Sdl.ri_name;
  Format.eprintf "Hardware acceleration: %b@\n"
    Sdl.Renderer.(test accelerated ri.ri_flags);
  Format.eprintf "Max texture size: %d x %d@\n" ri.Sdl.ri_max_texture_width
    ri.Sdl.ri_max_texture_height;
  Format.eprintf "Supported formats:@\n";
  List.iter
    (fun f -> Format.eprintf "  %s@\n%!" (Sdl.get_pixel_format_name f))
    ri.ri_texture_formats;
  Format.eprintf "Current video driver: %s@\n"
    (Option.value ~default:"" (Sdl.get_current_video_driver ()));
  Format.eprintf "Supported video drivers:@\n";
  for i = 0 to (result @@ Sdl.get_num_video_drivers ()) - 1 do
    Format.eprintf "  %s@\n%!" (result @@ Sdl.get_video_driver i)
  done

let finalize_texture txt = Gc.finalise Sdl.destroy_texture txt

let fold1 f l =
  match l with
  | [] -> None
  | r :: ll -> Some (List.fold_left f r ll)

let parse_flags flags =
  let win_flags, ren_flags =
    List.fold_left
      (fun ((wf, rf) as acc) f ->
         if String.length f == 0 then acc
         else if f.[0] == 'r' then (wf, f :: rf)
         else if f.[0] == 'w' then (f :: wf, rf)
         else acc)
      ([], []) flags
  in
  let win_flags =
    List.filter_map
      (fun f ->
         let open Sdl.Window in
         match f with
         | "w=fullscreen_desktop" -> Some fullscreen_desktop
         | "w=opengl" -> Some opengl
         | "w=shown" -> Some shown
         | "w=hidden" -> Some hidden
         | "w=borderless" -> Some borderless
         | "w=resizable" -> Some resizable
         | "w=minimized" -> Some minimized
         | "w=maximized" -> Some maximized
         | "w=input_grabbed" -> Some input_grabbed
         | "w=input_focus" -> Some input_focus
         | "w=mouse_focus" -> Some mouse_focus
         | "w=foreign" -> Some foreign
         | "w=allow_highdpi" -> Some allow_highdpi
         | "w=mouse_capture" -> Some mouse_capture
         | "w=always_on_top" -> Some always_on_top
         | "w=utility" -> Some utility
         | "w=popup_menu" -> Some popup_menu
         | "w=vulkan" -> Some vulkan
         | _ -> None)
      win_flags
  in
  let ren_flags =
    List.filter_map
      (fun f ->
         let open Sdl.Renderer in
         match f with
         | "r=software" -> Some software
         | "r=accelerated" -> Some accelerated
         | "r=presentvsync" -> Some presentvsync
         | "r=targettexture" -> Some targettexture
         | _ -> None)
      ren_flags
  in
  (fold1 Sdl.Window.( + ) win_flags, fold1 Sdl.Renderer.( + ) ren_flags)

(* We use premultiplied alpha blending everywhere *)
let pm_blend =
  Sdl.Blend.(
    Sdl.compose_custom_blend_mode one one_minus_src_alpha add one
      one_minus_src_alpha add)

let prepare_texture texture =
  result @@ Sdl.set_texture_blend_mode texture pm_blend;
  finalize_texture texture;
  texture

let create_texture renderer w h =
  let texture =
    result
    @@ Sdl.(
        create_texture renderer Pixel.format_argb8888 Texture.access_target ~w
          ~h)
  in
  let old = Sdl.get_render_target renderer in
  result @@ Sdl.set_render_target renderer (Some texture);
  let old_bm = result @@ Sdl.get_render_draw_blend_mode renderer in
  result @@ Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_none;
  result @@ Sdl.set_render_draw_color renderer 0 0 0 0;
  result @@ Sdl.render_clear renderer;
  result @@ Sdl.set_render_draw_blend_mode renderer old_bm;
  result @@ Sdl.set_render_target renderer old;
  prepare_texture texture

let create s =
  if not !initialized then begin
    (result @@ Sdl.(init Init.everything));
    initialized := true
  end;
  let title, w, h, flags = parse_window_spec s in
  let w_flags, r_flags = parse_flags flags in
  let w_flags =
    match w_flags with
    | None -> Sdl.Window.input_focus
    | Some f -> f
  in
  let window = result @@ Sdl.create_window title ~w ~h w_flags in
  let renderer =
    result
    @@
    match r_flags with
    | None -> Sdl.create_renderer window
    | Some flags -> Sdl.create_renderer ~flags window
  in
  let ri = result @@ Sdl.get_renderer_info renderer in
  let () = debug_ri_info ri in
  result @@ Sdl.set_render_draw_blend_mode renderer pm_blend;
  result @@ Sdl.render_set_logical_size renderer w h;
  ignore (Sdl.set_hint Sdl.Hint.render_scale_quality "1");
  let texture = create_texture renderer w h in
  result @@ Sdl.set_render_target renderer (Some texture);
  result @@ Sdl.render_set_logical_size renderer w h;
  let srect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0 in
  let drect = Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0 in
  let osrect = Some srect in
  let context =
    {
      renderer;
      texture;
      lwidth = w;
      lheight = h;
      srect;
      drect;
      osrect;
      angle = 0.0;
      flip = Sdl.Flip.none;
    }
  in
  { window; context }

let get_window_size win = Sdl.get_window_size win.window
let set_window_size win w h = Sdl.set_window_size win.window ~w ~h
let get_context win = win.context
let get_context_logical_size ctx = (ctx.lwidth, ctx.lheight)

let set_context_logical_size ctx w h =
  ctx.lwidth <- w;
  ctx.lheight <- h;
  result @@ Sdl.render_set_logical_size ctx.renderer w h

let set_transform ctx a h v =
  ctx.angle <- a;
  ctx.flip <-
    Sdl.Flip.((if h then horizontal else none) + if v then vertical else none)

let get_transform ctx =
  Sdl.Flip.
    ( ctx.angle,
      ctx.flip + horizontal = horizontal,
      ctx.flip + vertical = vertical )

let reset_transform ctx =
  ctx.angle <- 0.0;
  ctx.flip <- Sdl.Flip.none

let get_surface win = win.context.texture

let surface_size s =
  let _, _, size = result @@ Sdl.query_texture s in
  size

let create_surface ctx w h = create_texture ctx.renderer w h

let blit_full ctx rdst rsrc sx sy sw sh dx dy dw dh =
  let () =
    Sdl.Rect.(
      set_x ctx.srect sx;
      set_y ctx.srect sy;
      set_w ctx.srect sw;
      set_h ctx.srect sh;
      set_x ctx.drect dx;
      set_y ctx.drect dy;
      set_w ctx.drect dw;
      set_h ctx.drect dh)
  in
  let old = Sdl.get_render_target ctx.renderer in
  result @@ Sdl.set_render_target ctx.renderer (Some rdst);
  result
  @@ Sdl.render_copy_ex ~src:ctx.srect ~dst:ctx.drect ctx.renderer rsrc
    ctx.angle None ctx.flip;
  result @@ Sdl.set_render_target ctx.renderer old

let blit_scale ctx rdst rsrc dx dy dw dh =
  let () =
    Sdl.Rect.(
      set_x ctx.drect dx;
      set_y ctx.drect dy;
      set_w ctx.drect dw;
      set_h ctx.drect dh)
  in
  let old = Sdl.get_render_target ctx.renderer in
  result @@ Sdl.set_render_target ctx.renderer (Some rdst);
  result
  @@ Sdl.render_copy_ex ~dst:ctx.drect ctx.renderer rsrc ctx.angle None ctx.flip;
  result @@ Sdl.set_render_target ctx.renderer old

let blit ctx rdst rsrc dx dy =
  let sw, sh = surface_size rsrc in
  blit_scale ctx rdst rsrc dx dy sw sh

let color r g b a = (r, g, b, a)

let set_color ctx c =
  let r, g, b, a = c in
  result @@ Sdl.set_render_draw_color ctx.renderer r g b a

let fill_rect ctx rdst x y w h =
  let old = Sdl.get_render_target ctx.renderer in
  let () =
    Sdl.Rect.set_x ctx.srect x;
    Sdl.Rect.set_y ctx.srect y;
    Sdl.Rect.set_w ctx.srect w;
    Sdl.Rect.set_h ctx.srect h
  in
  result @@ Sdl.set_render_target ctx.renderer (Some rdst);
  result @@ Sdl.render_fill_rect ctx.renderer ctx.osrect;
  result @@ Sdl.set_render_target ctx.renderer old

let rec premultiply_alpha ba len i =
  if i < len then begin
    let b = Bigarray.Array1.unsafe_get ba i in
    let g = Bigarray.Array1.unsafe_get ba (i + 1) in
    let r = Bigarray.Array1.unsafe_get ba (i + 2) in
    let a = Bigarray.Array1.unsafe_get ba (i + 3) in
    let fa = float a /. 255. in
    Bigarray.Array1.unsafe_set ba i (int_of_float (float b *. fa));
    Bigarray.Array1.unsafe_set ba (i + 1) (int_of_float (float g *. fa));
    Bigarray.Array1.unsafe_set ba (i + 2) (int_of_float (float r *. fa));
    premultiply_alpha ba len (i + 4)
  end

let load_image ctx path =
  match Tsdl_image.Image.load path with
  | Ok surf ->
    let surf1 =
      result @@ Sdl.convert_surface_format surf Sdl.Pixel.format_argb8888
    in
    let () = Sdl.free_surface surf in
    result @@ Sdl.lock_surface surf1;
    let pix = Sdl.get_surface_pixels surf1 Bigarray.int8_unsigned in
    let len = Bigarray.Array1.dim pix in
    premultiply_alpha pix len 0;
    Sdl.unlock_surface surf1;
    let txt = result @@ Sdl.create_texture_from_surface ctx.renderer surf1 in
    let () = Sdl.free_surface surf1 in
    prepare_texture txt
  | Error (`Msg s) ->
    gfx_error "Cannot open image %s (internal error: %s)" path s

let init_ttf_sdl () =
  if not (Tsdl_ttf.Ttf.was_init ()) then result @@ Tsdl_ttf.Ttf.init ()

let load_font fn _ size =
  init_ttf_sdl ();
  let f = result @@ Tsdl_ttf.Ttf.open_font fn size in
  Gc.finalise Tsdl_ttf.Ttf.close_font f;
  f

let render_text ctx txt f =
  (* need to call load_font to get a font, so library is already initialized *)
  let r, g, b, a = result @@ Sdl.get_render_draw_color ctx.renderer in
  let col = Sdl.Color.create ~r ~g ~b ~a in
  let ssurf = result @@ Tsdl_ttf.Ttf.render_utf8_blended f txt col in
  result @@ Sdl.lock_surface ssurf;
  let pix = Sdl.get_surface_pixels ssurf Bigarray.int8_unsigned in
  let len = Bigarray.Array1.dim pix in
  premultiply_alpha pix len 0;
  Sdl.unlock_surface ssurf;
  let txt = result @@ Sdl.create_texture_from_surface ctx.renderer ssurf in
  Sdl.free_surface ssurf;
  prepare_texture txt

let measure_text txt fnt = result @@ Tsdl_ttf.Ttf.size_utf8 fnt txt

let get_key ev =
  String.lowercase_ascii @@ Sdl.get_key_name Sdl.Event.(get ev keyboard_keycode)

let running = ref true
let queue = Queue.create ()

let poll_event =
  let ev = Sdl.Event.create () in
  let se = Some ev in
  fun () ->
    while Sdl.poll_event se do
      let et = Sdl.Event.(get ev typ) in
      match Sdl.Event.enum et with
      | `Key_down -> Queue.add (Gfx_base.KeyDown (get_key ev)) queue
      | `Key_up -> Queue.add (Gfx_base.KeyUp (get_key ev)) queue
      | `Window_event
        when Sdl.Event.(get ev window_event_id = window_event_close) ->
        running := false
      | `Quit -> running := false
      | `Mouse_motion ->
        let x = Sdl.Event.(get ev mouse_motion_x) in
        let y = Sdl.Event.(get ev mouse_motion_y) in
        Queue.add (Gfx_base.MouseMove (x, y)) queue
      | `Mouse_button_up | `Mouse_button_down ->
        let button = Sdl.Event.(get ev mouse_button_button) in
        let state = Sdl.pressed = Sdl.Event.(get ev mouse_button_state) in
        let x = Sdl.Event.(get ev mouse_motion_x) in
        let y = Sdl.Event.(get ev mouse_motion_y) in
        Queue.add (Gfx_base.MouseButton (button, state, x, y)) queue

      | _ -> ()
    done;
    if Queue.is_empty queue then Gfx_base.NoEvent else Queue.take queue

let commit ctx =
  result @@ Sdl.set_render_target ctx.renderer None;
  result @@ Sdl.render_copy ctx.renderer ctx.texture;
  Sdl.render_present ctx.renderer;
  result @@ Sdl.set_render_target ctx.renderer (Some ctx.texture);
  result @@ Sdl.set_render_draw_color ctx.renderer 0 0 0 255;
  result @@ Sdl.render_clear ctx.renderer


let main_loop f k =
  let rec loop () =
    let ts = Int32.to_float (Sdl.get_ticks ()) in
    match f ts with
      None -> loop ()
    | Some res -> k res
  in
  loop ()

let open_formatter path =
  let oc = open_out path in
  let close f =
    Format.pp_print_flush f ();
    close_out oc
  in
  let fmt = Format.formatter_of_out_channel oc in
  Gc.finalise close fmt;
  fmt

let load_file path =
  let buff = Buffer.create 64 in
  let cin = open_in_bin path in
  try
    while true do
      Buffer.add_channel buff cin 64
    done;
    "" (* never reached *)
  with
  | End_of_file ->
    close_in cin;
    Buffer.contents buff