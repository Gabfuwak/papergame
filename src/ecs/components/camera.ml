type t = {
  mutable zoom: float;
  mutable width: float;
  mutable height: float;
  mutable target: Entity.t option;
  mutable offset: Vector.t; (* entity pos + offset = camera pos *)
}
