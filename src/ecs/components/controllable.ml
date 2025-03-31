type move =
      | Up
      | Down
      | Left
      | Right

type t = {
  mutable speed : float;
  mutable controls : (move, string) Hashtbl.t;
}
