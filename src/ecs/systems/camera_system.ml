open Types
open World
open Position
open Camera
open Vector

(*constants*)
let pos_easing_factor = 0.05
let zoom_easing_factor = 0.05
let max_zoom = 0.5
let min_zoom = 0.1
let zoom_margin = 500.0

let linear_easing_vec current_pos target_pos =
  let direction = Vector.sub target_pos current_pos in
  let movement = Vector.scale direction pos_easing_factor in
  Vector.add current_pos movement

let linear_easing current target =
  current +. (target -. current) *. zoom_easing_factor
  
  


let average_positions positions =
  if List.length positions = 0 then
    Vector.zero
  else
    let sum = List.fold_left Vector.add Vector.zero positions in
    Vector.scale sum (1.0 /. float_of_int (List.length positions))

(* Calculate dynamic zoom based on distance between targets *)
let calculate_zoom positions camera_width camera_height =
  match positions with
  | [] | [_] -> 1.0  (* Default zoom if 0 or 1 position *)
  | _ ->
      (* Find the min/max x and y coordinates to determine bounding box *)
      let min_x = ref max_float and max_x = ref min_float in
      let min_y = ref max_float and max_y = ref min_float in
      
      List.iter (fun pos ->
        min_x := min !min_x pos.x;
        max_x := max !max_x pos.x;
        min_y := min !min_y pos.y;
        max_y := max !max_y pos.y;
      ) positions;

      let width = (!max_x -. !min_x) +. zoom_margin in
      let height = (!max_y -. !min_y) +. zoom_margin in

      let width_zoom = camera_width /. width in
      let height_zoom = camera_height /. height in
      
      
      let ret = min (width_zoom) (height_zoom) in
      let lower_clamped_ret = max ret min_zoom in
      let upper_clamped_ret = min lower_clamped_ret max_zoom in
      upper_clamped_ret


let update world =
  match world.active_camera_id with
  | Some camera_id ->
      (match Hashtbl.find_opt world.state.camera_store camera_id,
             Hashtbl.find_opt world.state.position_store camera_id with
       | Some camera, Some camera_pos ->
           (match camera.target with
            | Some target_ids ->
                (* Collect positions of all valid targets *)
                let target_positions = List.filter_map
                  (fun target_id -> 
                     match Hashtbl.find_opt world.state.position_store target_id with
                     | Some pos -> Some pos.pos
                     | None -> None)
                  target_ids
                in
                if target_positions <> [] then
                  let avg_pos = average_positions target_positions in
                  let eased_pos = linear_easing_vec camera_pos.pos avg_pos in
                  camera_pos.pos <- Vector.add eased_pos camera.offset;
                  

                  let calculated_zoom = calculate_zoom target_positions camera.width camera.height in
                  let eased_zoom = linear_easing camera.zoom calculated_zoom in
                  camera.zoom <- eased_zoom;
                  Gfx.debug "Current zoom: %f\n" camera.zoom
            | None -> ())
       | _, _ -> ())
  | None -> ()
