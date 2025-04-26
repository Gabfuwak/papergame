type t = {
  mutable zoom: float;
  width: float;
  height: float;
  mutable target: Entity.t list option;
  mutable offset: Vector.t; (* entity pos + offset = camera pos *)
}
