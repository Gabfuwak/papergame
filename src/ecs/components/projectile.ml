type t = {
  mutable lifetime: float;         (* Time before auto-destruction in seconds *)
  mutable damage: int;             (* Damage amount *)
  mutable source_entity: int;      (* Entity that created this projectile *)
  mutable projectile_type: string; (* Type of projectile ("paint", etc.) *)
  mutable direction: float;        (* Direction multiplier (1.0 = right, -1.0 = left) *)
  mutable destroyed_on_hit: bool;  (* Whether this projectile is destroyed on hit *)
}
