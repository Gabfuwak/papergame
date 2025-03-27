open World
module P = Position
open Movable
open Collider
open Vector

let check_collision box1_pos box1_width box1_height box2_pos box2_width box2_height =
  let x1 = box1_pos.x in
  let y1 = box1_pos.y in
  let x2 = box2_pos.x in
  let y2 = box2_pos.y in

  let overlap_x =
    x1 < x2 +. box2_width &&
    x1 +. box1_width > x2
  in

  let overlap_y =
    y1 < y2 +. box2_height &&
    y1 +. box1_height > y2
  in

  overlap_x && overlap_y

let solve_collision is_controllable position movable collider1 collider2 collision_velocity=
  let safety_margin = 0.5 in

  if collider2.weight = Float.infinity then
    let box1 = collider1.boxes.(0) in
    let box2 = collider2.boxes.(0) in

    let box1_center_x = collider1.origin_pos.x +. box1.pos.x +. box1.width /. 2.0 in
    let box1_center_y = collider1.origin_pos.y +. box1.pos.y +. box1.height /. 2.0 in
    let box2_center_x = collider2.origin_pos.x +. box2.pos.x +. box2.width /. 2.0 in
    let box2_center_y = collider2.origin_pos.y +. box2.pos.y +. box2.height /. 2.0 in

    let dx = box1_center_x -. box2_center_x in
    let dy = box1_center_y -. box2_center_y in

    let penetration_x = box1.width /. 2.0 +. box2.width /. 2.0 -. abs_float dx in
    let penetration_y = box1.height /. 2.0 +. box2.height /. 2.0 -. abs_float dy in

    if penetration_x < penetration_y then (
      if is_controllable then
        movable.velocity.x <- collision_velocity.x 
      else
        movable.velocity.x <- collision_velocity.x -. movable.velocity.x;

      let correction = if dx > 0.0 then penetration_x +. safety_margin else -.(penetration_x +. safety_margin) in
      position.P.pos.x <- position.P.pos.x +. correction;
      collider1.origin_pos.x <- position.P.pos.x;
    )
    else (
      if is_controllable then
        movable.velocity.y <- collision_velocity.y
      else
        movable.velocity.y <- collision_velocity.y -.movable.velocity.y;

      let correction = if dy > 0.0 then penetration_y +. safety_margin else -.(penetration_y +. safety_margin) in
      position.P.pos.y <- position.P.pos.y +. correction;
      collider1.origin_pos.y <- position.P.pos.y;
    );
  ()

let update world =
  Hashtbl.iter (fun entity1 collider1 ->
    match (Hashtbl.find_opt world.state.movable_store entity1), (Hashtbl.find_opt world.state.position_store entity1) with
    | Some movable, Some position ->
      Hashtbl.iter (fun entity2 collider2 ->
        if entity1 <> entity2 then (
          let box1 = collider1.boxes.(0) in
          let box2 = collider2.boxes.(0) in

          let box1_pos = Vector.add collider1.origin_pos box1.pos in
          let box2_pos = Vector.add collider2.origin_pos box2.pos in

          if check_collision box1_pos box1.width box1.height box2_pos box2.width box2.height then (
            Gfx.debug "Collision detected between %d and %d!\n" entity1 entity2;
            let is_controllable = Hashtbl.mem world.state.controllable_store entity1 in
            let movable_opt= Hashtbl.find_opt world.state.movable_store entity2 in
            let collision_velocity = 
              match movable_opt with 
              | Some a -> a.velocity
              | None -> Vector.zero
            in
            solve_collision is_controllable position movable collider1 collider2 collision_velocity;
          )
        )
      ) world.state.collider_store;
    | _, _ -> ()
  ) world.state.collider_store;
