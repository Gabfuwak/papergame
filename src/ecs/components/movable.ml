type t = { 
  mutable velocity : Vector.t;
  (* forces are acceleration *)
  mutable force : Vector.t;
  mutable gravity_scale : float; (* 0.0 for no gravity, 1.0 for normal gravity *)
}
