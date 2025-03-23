exception GfxError of string
(** All errors are reported with this exception. The payload is an informative
    message. *)

type window
(** Type of windows.
    - JavaScript : represents the initial canvas element.
    - SDL : represent a top-level window. *)

type surface
(** Type of surfaces. These are rectangles of pixels onto which
    one can draw, blit, etc… *)

type context
(** Type of rendering context. *)

type color
(** Type of colors. *)

type font
(** Type of font objects. *)

type 'a resource
(** Remote resource. *)

val resource_ready : 'a resource -> bool
(** Tests if the resource is available. *)

val get_resource : 'a resource -> 'a
(** Returns the content of the resource.
    @raises Failure with an appropriate message if the resource is not ready. *)

val get_resource_opt : 'a resource -> 'a option
(** Returns [Some v] if the resource is ready and [None] otherwise. *)

val backend : string
(** [backend] is the name of the backend. It can be ["sdl"] or ["js"]. *)

val create : string -> window
(** [create s] returns a window and a rendering surface from the string [s].
    The string has the form ["name:WxH:flags"].
    - JavaScript : [name] is element id of the canvas representing the window.
    - SDL : [name] is the window title. *)

val get_window_size : window -> int * int
(** returns the dimensions in screen pixels of the window. *)

val set_window_size : window -> int -> int -> unit
(** sets the dimensions in pixels of the window. *)

val get_context : window -> context
(** [get_context w] returns the context of window [w]. *)


val set_context_logical_size : context -> int -> int -> unit
(** [set_context_logical_size ctx w h] sets the logical size of the context.
    The initial values are the same dimentions as the window. The logical size
    reflects the range of pixels that are shown in the context. For instance, If
    the logical size is 100x100 but the window size is 400x400, each logical
    pixel will be automatically zoomed and displayed as a 4x4 pixel in the
    window. *)

val get_context_logical_size : context -> int * int
(** [get_context_logical_size ctx w h] returns the logical size of the
    context. *)

val set_transform : context -> float -> bool -> bool -> unit
(** [set_transform ctx angle hflip vflip] stores a transformation in the
    context. The transformation is a rotation of [angle] (in radians), on
    horizontal reflection (if [hflip] is [true]) and a vertical reflection (if
    [vflip]) is [true]). *)


val get_transform : context -> float * bool * bool
(** [get_transform ctx] returns the transformation currently associated with the
    context. *)

val reset_transform : context -> unit
(** [reset_transform ctx] is an alias for
    [set_transform ctx 0.0 false false]. *)

val get_surface : window -> surface
(** [get_surface w] returns the underlying surface of window [w]. *)

val create_surface : context -> int -> int -> surface
(** [create_surface ctx w h] creates a surface for the given rendering
    context. *)

val surface_size : surface -> int * int
(** returns the dimensions of a surface. *)

val blit : context -> surface -> surface -> int -> int -> unit
(** [blit dst src x y] copies surface [src] on surface [dst] at point
    [(x,y)]. *)

val blit_scale :
  context -> surface -> surface -> int -> int -> int -> int -> unit
(** [blit_scale ctx dst src dx dy dw dh] copies surface [src] on surface [dst]
      at point [(dx,dy)] scaling it to [dw] width and [dh] height *)

val blit_full :
  context -> surface -> surface ->
  int -> int -> int -> int ->
  int -> int -> int -> int ->
  unit
(** [blit_full ctx dst src sx sy sw sh dx dy dw dh] copies the surface extracted
    from [src] at point [(sx, sy)] with dimensions [(sw, sh)] on surface [dst]
    at point [(dx,dy)] scaling it to [dw] width and [dh] height. *)

val color : int -> int -> int -> int -> color
(** [color r g b a] returns a color built from components red green blue and
    alpha. all values must be integers between 0 and 255 inclusive. *)

val set_color : context -> color -> unit
(** [set_color ctx c] sets the current color to [c]. *)

val fill_rect : context -> surface -> int -> int -> int -> int -> unit
(** [fill_rect ctx dst x y w h] draws and fills a rectangle on surface surface
    [dst] at coordinates [(x, y)] and with dimensions [w * h]. The rectangle is
    filled with current color. *)

val load_image : context -> string -> surface resource
(** [load_image ctx path] loads an image whose content is given by an
    implementation dependent string (filename, url, … ).  Common image types are
    supported (PNG, JPEG, …).  The returned resource may not be extracted used
    until [resource_ready] returns [true]. *)

val load_file : string -> string resource
(** [load_file path] creates a resource that, when ready, resolves to
    the content of the file denoted by path.
    - JavaScript : [path] is a URL to be loaded with.
    - SDL : [path] is the path of a file. *)

val load_font : string -> string -> int -> font
(** [load_font fn extra size] loads font [fn] at size [size] given in points.
    The [extra] parameters allows to pass implementation dependent options.
    - JavaScript : [fn] is a font name. If it does not exist, it is silently
      replaced by a close matching font or default font by the browser.
    - SDL : [fn] must be a path to the [.ttf] file containing the font. *)

val render_text : context -> string -> font -> surface
(** [render_text ctx txt f c] returns a surface containing the text
    [txt] rendered using font [f] with color [c]. *)

val measure_text : string -> font -> int * int
(** [mesure_text txt f] returns the size (width and height) of the surface that
    [render_text] would return, without creating it. *)

type event =
  | NoEvent (** no event *)
  | KeyUp of string (** Key with a given name was released *)
  | KeyDown of string (** Key with a given name was pressed *)
  | MouseMove of int * int (** button pressed bitmask and x/y coordinates, relative to the window. *)
  | MouseButton of int * bool * int * int (** button button number, pressed/released, x/y relative to the window. *)
  | Quit (** returned by the SDL backend whenever the user closes the window or hit CTRL-C in the terminal *)

(** The type of input events.
    The string describing keyboard events is implementation defined. *)

val poll_event : unit -> event
(** [poll_event ()] returns the next event in the event queue. *)

val main_loop : ?limit:bool -> (float -> 'a option) -> ('a -> unit) -> unit
(** [main_loop f k] calls a [f] repeteadly. If the optional parameter [limit] is [true] (the default)
    then [f] is called no faster than 60 times/second. If [limit] is [false] then the function
    is called as much as possible which means:
    - SDL : [f] is called as fast as possible, unless the flag ["r=presentvsync"] is passed as
      a flag when creating the window, in which case, the function is called at the monitor refresh rate.
    - JS : [f] is called at the monitor refresh rate (which may be more than 60Hz) as per the specification of [requestAnimationFrame].
      The callback [f] is given a float representing the elapsed time in
      milliseconds since the begining of the program.
      if [f] returns [None], then it will continue being called.
      If [f] returns [Some v] then the continuation [k v] is called instead (once).

    One would typically write code such as:
    {[
      let text_file = Gfx.load_file "foo.txt" in
      Gfx.main_loop
        (fun _dt -> Gfx.get_resource_opt text_file)
        (fun content ->
           (* do something with the content *)
        )
    ]}
*)

val commit : context -> unit
(** [commit ctx] renders the rendering context on to its underlying window.
    This should be the last graphical operation of the current frame. *)

val open_formatter : string -> Format.formatter
(** [open_formatter src] opens a formatter for debugging purposes.
    - JavaScript : [src] must be the ID of an element whose innerHTML is
      appended to. It is recommended to style the element with the CSS property
      white-space:pre to preserve white spaces.
    - SDL : [src] is the file path. *)

val set_debug_formatter : Format.formatter -> unit
(** [set_debug_formatter fmt] sets the current formatter.
    The default value is Format.stderr.
    - JavaScript : writing to [Format.stderr] writes to the JavaScript console.
      in error mode ([console.error]).
    - SDL : writing to [Format.stderr] writes to the standard error as usual. *)

val debug : ('a, Format.formatter, unit) format -> 'a
(** [debug f a b c d...] prints to the currently configured debug formatter.
    The first argument [f] is a format string (see {!Format.printf}). *)
