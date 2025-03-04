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

let solve_collision position movable collider1 collider2 =
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
      movable.velocity.x <- -.movable.velocity.x;

      let correction = if dx > 0.0 then penetration_x else -.penetration_x in
      position.P.pos.x <- position.P.pos.x +. correction;
      collider1.origin_pos.x <- position.P.pos.x;
    )
    else (
      movable.velocity.y <- -.movable.velocity.y;

      let correction = if dy > 0.0 then penetration_y else -.penetration_y in
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
            solve_collision position movable collider1 collider2;
          )
        )
      ) world.state.collider_store;
    | _, _ -> ()
  ) world.state.collider_store;
