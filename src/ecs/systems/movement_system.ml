open World
open Position
open Movable
open Vector

let update world =
  let dt = world.dt *. 0.001 in
  
  Hashtbl.iter (fun entity movable ->
    match Hashtbl.find_opt world.state.position_store entity with
    | Some position ->
        let displacement = Vector.scale movable.velocity dt in
        position.pos <- Vector.add position.pos displacement;
        
        (match Hashtbl.find_opt world.state.collider_store entity with
        | Some collider -> collider.origin_pos <- position.pos
        | None -> ()
        )
    | _ -> ()
  ) world.state.movable_store
