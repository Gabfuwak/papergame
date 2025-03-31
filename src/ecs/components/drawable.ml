open Types
open Vector

type t = {
  screen_pos: Vector.t;
  width: int;
  height: int;
  mutable texture: Types.texture;
}
