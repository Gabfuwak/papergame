type move =
      | Up
      | Down
      | Left
      | Right
      | Jump

type t = {
  mutable controls : (move, string) Hashtbl.t;
}
