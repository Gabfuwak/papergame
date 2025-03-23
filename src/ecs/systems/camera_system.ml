open Types
open World
open Position
open Camera
open Vector

let linear_easing current_pos target_pos =
  let easing_factor = 0.05 in
  let direction = Vector.sub target_pos current_pos in
  let movement = Vector.scale direction easing_factor in
  Vector.add current_pos movement

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
                     let eased_pos = linear_easing camera_pos.pos target_pos.pos in
                     camera_pos.pos <- Vector.add eased_pos camera.offset
                 | None -> ())
            | None -> ())
       | _, _ -> ())
  | None -> ()
