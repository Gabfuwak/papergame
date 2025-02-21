(* Exception raised when a graphical error occurs *)
exception GfxError of string

let gfx_error fmt = Format.kasprintf (fun msg -> raise (GfxError msg)) fmt

(* Type of user input events *)
type event =
  | NoEvent (* no event *)
  | KeyUp of string (* Key with a given name was released *)
  | KeyDown of string (* Key with a given name was pressed *)
  | MouseMove of int * int (*x/y coordinates, relative to the window. *)
  | MouseButton of int * bool * int * int (* button button number, pressed/released, x/y relative to the window. *)

let parse_window_spec s =
  try
    Scanf.sscanf s "%[^:]:%dx%d:%s" (fun title width height flags ->
        (title, width, height, String.split_on_char ',' flags))
  with
  | Scanf.Scan_failure msg -> begin
      match String.split_on_char ':' msg with
      | _ :: _ :: l ->
          gfx_error "Invalid window spec `%s` : %s" s (String.concat "" l)
      | _ -> gfx_error "Invalid window spec `%s`" s
    end
  | End_of_file -> gfx_error "Invalid window spec `%s`" s


let debug_formatter = ref Format.err_formatter

let set_debug_formatter fmt = debug_formatter := fmt

let debug str = Format.fprintf !debug_formatter str