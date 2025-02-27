open Types

type t = {
  mutable screen_pos : Vector.t;
  height : int;
  width : int;
  mutable tex : texture;
}
