type hitbox = {
  mutable boxtype : string; (* this is for when we'll implement attacks, we have a attack hitbox, a damage taking hitbox etc..*)
  mutable pos : Vector.t; (* position of this box relative to the collider origin *)
  mutable height : float;
  mutable width : float;
}

type t = {
  mutable origin_pos : Vector.t;
  mutable boxes : hitbox array;
  mutable weight : float;
}
