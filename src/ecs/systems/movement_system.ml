open World
open Position
open Movable
open Vector

let update world =
  (* Find all movable entities *)
  Hashtbl.iter (fun entity movable ->
    match Hashtbl.find_opt world.state.position_store entity with
    | Some position ->
        position.pos <- Vector.add position.pos movable.velocity;
        (match Hashtbl.find_opt world.state.collider_store entity with
        | Some collider -> collider.origin_pos <- position.pos
        | None -> ()
        )
    | _ -> ()
  ) world.state.movable_store
