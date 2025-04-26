open World
open Position
open Movable
open Vector

let gravity = Vector.create 0.0 980.0 (* I consider that 1 unit = 1cm *)

let update world =
  let dt = world.dt *. 0.001 in
  Hashtbl.iter (fun entity movable ->
    (match Hashtbl.find_opt world.state.collider_store entity with
    | Some collider when collider.weight < Float.infinity ->
        movable.force <- Vector.add movable.force (Vector.scale gravity movable.gravity_scale)
    | _ -> ());
    
    movable.velocity <- Vector.add movable.velocity (Vector.scale movable.force dt);
    movable.force <- Vector.zero
  ) world.state.movable_store
