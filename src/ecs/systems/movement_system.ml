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
    | _ -> ()
  ) world.state.movable_store
