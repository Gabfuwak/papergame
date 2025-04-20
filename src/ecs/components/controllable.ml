type move =
      | Up
      | Down
      | Left
      | Right
      | Jump
      | Attack of {attack_type: int}

type t = {
  mutable controls : (move, string) Hashtbl.t;
}
