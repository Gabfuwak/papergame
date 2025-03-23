open World
open Position
open Movable
open Vector

let gravity = Vector.create 0.0 9.8

let update world =
  let dt = world.dt *. 0.001 in
  
  Hashtbl.iter (fun entity movable ->
    (match Hashtbl.find_opt world.state.collider_store entity with
    | Some collider when collider.weight < Float.infinity ->
        movable.force <- Vector.add movable.force gravity
    | _ -> ());
    
    movable.velocity <- Vector.add movable.velocity (Vector.scale movable.force dt);
    
    movable.force <- Vector.create 0.0 0.0
  ) world.state.movable_store
