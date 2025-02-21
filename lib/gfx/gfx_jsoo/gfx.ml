open Js_of_ocaml
include Gfx_base

let backend = "js"
let loaded = ref false

let () =
  Dom_html.window##.onload :=
    Dom_html.handler (fun _ ->
        loaded := true;
        Js._true)

type window = Dom_html.canvasElement Js.t
type context = Dom_html.canvasRenderingContext2D Js.t
type surface = Dom_html.canvasElement Js.t
type color = Js.js_string Js.t
type font = Js.js_string Js.t

type 'a resource = {
  get : unit -> 'a;
  is_ready : unit -> bool;
}

let resource_ready r = r.is_ready ()
let get_resource r = r.get ()
let get_resource_opt r =
  if r.is_ready () then Some (r.get()) else None
let events = Queue.create ()
let get_context w = w##getContext Dom_html._2d_

let get_mouse_event w h ev =
  let x = min (w-1) (max 0 ev##.offsetX) in
  let y = min (h-1) (max 0 ev##.offsetY) in
  let button = ev##.button in
  button, x, y
let create s =
  let id, w, h, _ = parse_window_spec s in
  match Dom_html.getElementById_coerce id Dom_html.CoerceTo.canvas with
  | None -> gfx_error "Cannot find canvas with id `%s`" id
  | Some canvas ->
    canvas##.width := w;
    canvas##.height := h;
    canvas##.style##.width := Js.string (string_of_int w ^ "px");
    canvas##.style##.height := Js.string (string_of_int h ^ "px");
    Dom_html.window##.onkeydown
    := Dom_html.handler (fun e ->
        Js.Optdef.iter e##.key (fun k ->
            Queue.add (Gfx_base.KeyDown (Js.to_string k)) events);
        Js._true);
    Dom_html.window##.onkeyup :=
      Dom_html.handler (fun e ->
          Js.Optdef.iter e##.key (fun k ->
              Queue.add (Gfx_base.KeyUp (Js.to_string k)) events);
          Js._true);
    canvas##.onmousemove :=
      Dom_html.handler (fun e ->
          Dom.preventDefault e;
          let _, x, y = get_mouse_event w h e in
          Queue.add (Gfx_base.MouseMove (x, y)) events;
          Js._true);
    canvas##.onmousedown :=
      Dom_html.handler (fun e ->
          Dom.preventDefault e;
          let button, x, y = get_mouse_event w h e in
          Queue.add (Gfx_base.MouseButton (button, true, x, y)) events;
          Js._true);
    canvas##.onmouseup :=
      Dom_html.handler (fun e ->
          Dom.preventDefault e;
          let button, x, y = get_mouse_event w h e in
          Queue.add (Gfx_base.MouseButton (button, false, x, y)) events;
          Js._true);
    canvas##.onmouseover :=
      Dom_html.handler (fun e ->
          canvas##focus;
          Js._true);
    ignore (Dom_html.addEventListener canvas (Dom_events.Typ.make "contextmenu")
              (Dom_html.handler (fun e -> Dom.preventDefault e; Js._false)) Js._true);
    canvas##focus;
    canvas

let get_window_size w = (w##.clientWidth, w##.clientHeight)

let set_window_size canvas w h =
  canvas##.style##.width := Js.string (string_of_int w ^ "px");
  canvas##.style##.height := Js.string (string_of_int h ^ "px")

let get_context_logical_size ctx = (ctx##.canvas##.width, ctx##.canvas##.height)

let set_context_logical_size ctx w h =
  ctx##.canvas##.width := w;
  ctx##.canvas##.height := h

let get_transform ctx : float * bool * bool =
  let a : float = Js.Unsafe.get ctx (Js.string "__angle__") in
  let h : float = Js.Unsafe.get ctx (Js.string "__hflip__") in
  let v : float = Js.Unsafe.get ctx (Js.string "__vflip__") in
  (a /. 0.017453292519943295, h < 0.0, v < 0.0)

let set_transform ctx a h v =
  Js.Unsafe.set ctx (Js.string "__angle__") (a *. 0.017453292519943295);
  Js.Unsafe.set ctx (Js.string "__hflip__") (if h then -1.0 else 1.0);
  Js.Unsafe.set ctx (Js.string "__vflip__") (if v then -1.0 else 1.0)

let reset_transform ctx =
  Js.Unsafe.delete ctx (Js.string "__angle__");
  Js.Unsafe.delete ctx (Js.string "__hflip__");
  Js.Unsafe.delete ctx (Js.string "__vflip__")

let get_surface w = w

let create_surface _ctx w h =
  let doc = Dom_html.document in
  let canvas = Dom_html.createCanvas doc in
  canvas##.height := h;
  canvas##.width := w;
  canvas

let surface_size (s : surface) = (s##.width, s##.height)

let blit_full _ctx (dst : surface) (src : surface) sx sy sw sh dx dy dw dh =
  let ctx = dst##getContext Dom_html._2d_ in
  let fdx = float dx in
  let fdy = float dy in
  let fdw = float dw in
  let fdh = float dh in
  let a = Js.Unsafe.get ctx (Js.string "__angle__") in
  let tst = Js.Optdef.test a in
  if tst then begin
    let tx = fdx +. (0.5 *. fdw) in
    let ty = fdy +. (0.5 *. fdh) in
    ctx##save;
    if a != 0.0 then begin
      ctx##translate tx ty;
      ctx##rotate a;
      ctx##translate (-.tx) (-.ty)
    end;
    let cx : float = Js.Unsafe.get ctx (Js.string "__hflip__") in
    let cy : float = Js.Unsafe.get ctx (Js.string "__vflip__") in
    if cx < 0.0 || cy < 0.0 then begin
      let tx = if cx < 0.0 then tx else 0.0 in
      let ty = if cy < 0.0 then ty else 0.0 in
      ctx##translate tx ty;
      ctx##scale cx cy;
      ctx##translate (-.tx) (-.ty)
    end
  end;
  ctx##drawImage_fullFromCanvas
    src (float sx) (float sy) (float sw) (float sh) fdx fdy fdw fdh;
  if tst then ctx##restore

let blit _ctx (dst : surface) (src : surface) x y =
  blit_full _ctx dst src 0 0 src##.width src##.height x y src##.width
    src##.height

let blit_scale _ctx (dst : surface) (src : surface) dx dy dw dh =
  blit_full _ctx dst src 0 0 src##.width src##.height dx dy dw dh

let digit n =
  let n = n land 0xf in
  Char.chr (n + if n < 10 then 48 else 55)

let buff = Bytes.create 9
(* we are in a single threaded context because of JavaScript *)

let color r g b a =
  Bytes.set buff 0 '#';
  Bytes.set buff 1 (digit (r lsr 4));
  Bytes.set buff 2 (digit r);
  Bytes.set buff 3 (digit (g lsr 4));
  Bytes.set buff 4 (digit g);
  Bytes.set buff 5 (digit (b lsr 4));
  Bytes.set buff 6 (digit b);
  Bytes.set buff 7 (digit (a lsr 4));
  Bytes.set buff 8 (digit a);
  Js.string (Bytes.unsafe_to_string buff)

let set_color (ctx : context) c = ctx##.fillStyle := c

let get_fillStyle ctx : Js.js_string Js.t =
  (* fillStyle is set writeOnly in jsoo binding *)
  Js.Unsafe.get ctx (Js.string "fillStyle")

let fill_rect _ctx dst x y w h =
  let ctx = dst##getContext Dom_html._2d_ in
  ctx##.fillStyle := get_fillStyle _ctx;
  ctx##fillRect (float x) (float y) (float w) (float h)

let load_image ctx src =
  let canvas = create_surface ctx 0 0 in
  let img = Dom_html.createImg Dom_html.document in
  img##.src := Js.string src;
  let is_ready () = Js.to_bool img##.complete in
  let get () =
    if not (is_ready ()) then failwith "Image is not ready";
    canvas##.width := img##.width;
    canvas##.height := img##.height;
    (canvas##getContext Dom_html._2d_)##drawImage img 0.0 0.0;
    canvas
  in
  { get; is_ready }

let load_font fn extra size =
  let extra = if extra <> "" then extra ^ " " else extra in
  let s = Js.string (extra ^ string_of_int size ^ "px " ^ fn) in
  s

let measure_text_ctx ctx text font =
  ctx##.font := font;
  ctx##.textBaseline := Js.string "top";
  let m = ctx##measureText (Js.string text) in
  let w = int_of_float (m##.width +. 1.0) in
  let m = ctx##measureText (Js.string "M") in
  let h = int_of_float (m##.width *. 1.8) in
  (w, h)

let measure_text text font =
  measure_text_ctx
    ((Dom_html.createCanvas Dom_html.document)##getContext Dom_html._2d_)
    text font

let render_text _ctx text font =
  let canvas = create_surface _ctx 0 0 in
  let ctx = canvas##getContext Dom_html._2d_ in
  (* sets the baseline and the font *)
  let w, h = measure_text_ctx ctx text font in
  canvas##.width := w;
  canvas##.height := h;
  ctx##.fillStyle := get_fillStyle _ctx;
  ctx##.font := font;
  ctx##.textBaseline := Js.string "top";
  ctx##fillText (Js.string text) 0.0 (float h *. 0.1);
  canvas

let poll_event () =
  if Queue.is_empty events then Gfx_base.NoEvent else Queue.pop events


let main_loop f k =
  let cb = ref (Js.wrap_callback (fun _ -> assert false)) in
  let loop dt =
    match f dt with
      None -> ignore (Dom_html.window##requestAnimationFrame !cb)
    | Some res -> (k res)
  in
  cb := Js.wrap_callback loop;
  ignore (Dom_html.window##requestAnimationFrame !cb)

let commit _ = ()

let rec equal_between pat lpat str i =
  i >= lpat || (pat.[i] = str.[i] && equal_between pat lpat str (i + 1))

let starts_with pat str =
  (* only present in OCaml >= 4.13 *)
  let lpat = String.length pat in
  let lstr = String.length str in
  lpat <= lstr && equal_between pat lpat str 0

let load_from_var var =
  let s : Js.js_string Js.t Js.Optdef.t =
    Js.Unsafe.get Dom_html.window (Js.string var)
  in
  let is_ready () = true in
  let get () =
    Js.to_string
      (Js.Optdef.get s (fun () ->
           failwith ("Undefined property " ^ var ^ " on the global object.")))
  in
  { get; is_ready }

let load_from_storage key =
  let storage =
    Js.Optdef.get Dom_html.window##.localStorage (fun () ->
        failwith "STORAGE scheme is unsupported")
  in
  let is_ready () = true in
  let get () =
    let str =
      Js.Opt.get
        (storage##getItem (Js.string key))
        (fun () -> failwith ("Undefined localStorage entry " ^ key))
    in
    Js.to_string str
  in
  { is_ready; get }

let chop_prefix pre s =
  assert (starts_with pre s);
  let lpre = String.length pre in
  String.sub s lpre (String.length s - lpre)

let load_file url_s =
  if starts_with "VAR:" url_s then load_from_var (chop_prefix "VAR:" url_s)
  else if starts_with "STORAGE:" url_s then
    load_from_storage (chop_prefix "STORAGE:" url_s)
  else
    let url = Js.string url_s in
    let xhr = XmlHttpRequest.create () in
    let () = xhr##_open (Js.string "GET") url Js._true in
    let is_ready () =
      match xhr##.readyState with
      | XmlHttpRequest.DONE -> true
      | _ -> false
    in
    let get () =
      match xhr##.readyState with
      | XmlHttpRequest.DONE ->
        Js.to_string (Js.Opt.get xhr##.responseText (fun () -> Js.string ""))
      | _ -> failwith ("URL: " ^ url_s ^ " is not ready")
    in
    xhr##send Js.null;
    { is_ready; get }

let open_formatter src =
  try
    let div = Dom_html.getElementById src in
    let out_string s p n =
      let ss = String.sub s p n in
      let ss =
        match String.index ss '\x00' with
        | idx ->
          div##.innerHTML := Js.string "";
          let i = idx + 1 in
          String.sub ss i (String.length ss - i)
        | exception Not_found -> ss
      in
      div##.innerHTML := div##.innerHTML##concat (Js.string ss)
    in
    let out_flush () = () in
    let out_newline () = out_string "\n" 0 1 in
    let out_spaces n =
      for i = 0 to n - 1 do
        out_string " " 0 1
      done
    in
    let out_indent = out_spaces in
    Format.(
      formatter_of_out_functions
        { out_string; out_flush; out_newline; out_spaces; out_indent })
  with
  | Not_found -> failwith ("No element with id " ^ src)
