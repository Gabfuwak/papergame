open Types
open World
open Position
open Camera
open Vector

let update world =
  match world.active_camera_id with
  | Some camera_id ->
      (match Hashtbl.find_opt world.state.camera_store camera_id,
             Hashtbl.find_opt world.state.position_store camera_id with
       | Some camera, Some camera_pos ->
           (match camera.target with
            | Some target_id ->
                (match Hashtbl.find_opt world.state.position_store target_id with
                 | Some target_pos ->
                     camera_pos.pos <- Vector.add target_pos.pos camera.offset
                 | None -> ())
            | None -> ())
       | _, _ -> ())
  | None -> ()
