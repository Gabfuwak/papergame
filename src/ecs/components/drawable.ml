open Types

type t = {
  mutable texture: Types.texture;
}


let get_reference drawable =
  match drawable.texture with
  | Types.Image { reference; _ } -> reference
  | Types.Animation { reference; _ } -> reference
  | _ -> Vector.zero
