open World
module P = Position
open Movable
open Collider
open Vector
open State.Character

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

let solve_collision world entity1 is_controllable position movable collider1 collider2 collision_velocity =
  let safety_margin = 0.5 in

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

    if dy < 0.0 then ( (* We're colliding from above *)
      (match Hashtbl.find_opt world.state.character_store entity1 with
      | Some character ->
          (*if not character.is_grounded then Gfx.debug "Character grounded.\n" else ();*)
          character.is_grounded <- true;
      | None -> ())
    )
  );
  ()

let check_solid_collisions world =
  Hashtbl.iter (fun entity1 collider1 ->
    match (Hashtbl.find_opt world.state.movable_store entity1), (Hashtbl.find_opt world.state.position_store entity1) with
    | Some movable, Some position ->
      Hashtbl.iter (fun entity2 collider2 ->
        if entity1 <> entity2 && collider2.weight = Float.infinity then (
          (* Use first box for now - can be enhanced to check specific types *)
          let box1 = collider1.boxes.(0) in
          let box2 = collider2.boxes.(0) in

          let box1_pos = Vector.add collider1.origin_pos box1.pos in
          let box2_pos = Vector.add collider2.origin_pos box2.pos in

          if check_collision box1_pos box1.width box1.height box2_pos box2.width box2.height then (
            let is_controllable = Hashtbl.mem world.state.controllable_store entity1 in
            let movable_opt = Hashtbl.find_opt world.state.movable_store entity2 in
            let collision_velocity =
              match movable_opt with
              | Some a -> a.velocity
              | None -> Vector.zero
            in
            solve_collision world entity1 is_controllable position movable collider1 collider2 collision_velocity;
          )
        )
      ) world.state.collider_store;
    | _, _ -> ()
  ) world.state.collider_store


let check_attack_collisions world =
  Hashtbl.iter (fun attacker_id attacker_collider ->
    match Hashtbl.find_opt world.state.character_store attacker_id with
    | Some attacker_char ->
        (match attacker_char.current_state with
        | Attacking { attack_type } ->
            Hashtbl.iter (fun defender_id defender_collider ->
              if attacker_id <> defender_id then
                match Hashtbl.find_opt world.state.character_store defender_id with
                | Some defender_char ->
                    (match defender_char.current_state with
                    | Hit _ -> ()
                    | _ ->
                        let collision_detected = ref false in
                        Array.iter (fun attack_box ->
                          if attack_box.boxtype = "attack" && not !collision_detected then
                            Array.iter (fun vulnerable_box ->
                              if vulnerable_box.boxtype = "vulnerable" && not !collision_detected then
                                let attack_box_pos = Vector.add attacker_collider.origin_pos attack_box.pos in
                                let vulnerable_box_pos = Vector.add defender_collider.origin_pos vulnerable_box.pos in

                                if check_collision
                                    attack_box_pos attack_box.width attack_box.height
                                    vulnerable_box_pos vulnerable_box.width vulnerable_box.height then
                                  begin
                                    collision_detected := true;

                                    let base_x = if attacker_char.facing_right then 400.0 else -400.0 in
                                    let hit_vector = match attack_type with
                                      | 0 -> Vector.create base_x (-200.0) (* Forward attack *)
                                      | 1 -> Vector.create (base_x *. 0.5) (-400.0) (* Up attack *)
                                      | _ -> Vector.create base_x (-200.0)
                                    in

                                    defender_char.pending_hit <- Some (hit_vector, 0.5)
                                  end
                            ) defender_collider.boxes
                        ) attacker_collider.boxes
                    )
                | None -> ()
            ) world.state.collider_store
        | _ -> ())
    | None -> ()
  ) world.state.collider_store

(* Main update function *)
let update world =
  (* First handle regular solid collisions *)
  check_solid_collisions world;
  
  (* Then check for attack collisions *)
  check_attack_collisions world
