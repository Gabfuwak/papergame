open Types
open World
open Vector
open Projectile
module Col = Collider
open State
open Character

let remove_entity world entity =
  Hashtbl.remove world.state.position_store entity;
  Hashtbl.remove world.state.movable_store entity;
  Hashtbl.remove world.state.drawable_store entity;
  Hashtbl.remove world.state.collider_store entity;
  Hashtbl.remove world.state.projectile_store entity

(* Simplify the collision detection to avoid nested pattern matching *)
let check_collision_for_projectile world projectile_id projectile projectile_collider =
  Hashtbl.iter (fun target_id target_char ->
    (* Skip collision with source entity or entities currently in Hit state *)
    if target_id <> projectile.source_entity then
      match Hashtbl.find_opt world.state.collider_store target_id, target_char.current_state with
      | Some target_collider, Hit _ -> 
          (* Skip targets already in hit state *)
          ()
      | Some target_collider, _ ->
          let collision_detected = ref false in
          
          (* Check each projectile box against each target box *)
          Array.iter (fun attack_hitbox ->
            if attack_hitbox.Col.boxtype = "attack" && not !collision_detected then
              Array.iter (fun vulnerable_hitbox ->
                if vulnerable_hitbox.Col.boxtype = "vulnerable" && not !collision_detected then begin
                  let attack_box_pos = Vector.add projectile_collider.Col.origin_pos attack_hitbox.Col.pos in
                  let vulnerable_box_pos = Vector.add target_collider.Col.origin_pos vulnerable_hitbox.Col.pos in

                  
                  if Collision_system.check_collision
                      attack_box_pos attack_hitbox.Col.width attack_hitbox.Col.height
                      vulnerable_box_pos vulnerable_hitbox.Col.width vulnerable_hitbox.Col.height then begin
                    collision_detected := true;
                  Gfx.debug "Projectile %d source: %d, checking target: %d\n" projectile_id projectile.source_entity target_id;
                    
                    (* Apply hit to the target character *)
                    let hit_direction = projectile.direction in
                    let base_x = if hit_direction > 0.0 then 300.0 else -300.0 in
                    let hit_vector = Vector.create base_x (-200.0) in
                    
                    target_char.pending_hit <- Some (hit_vector, 0.5);
                    
                    (* Mark projectile for deletion if it should be destroyed on hit *)
                    if projectile.destroyed_on_hit then
                      projectile.lifetime <- 0.0
                  end
                end
              ) target_collider.Col.boxes
          ) projectile_collider.Col.boxes
      | None, _ -> ()
  ) world.state.character_store

let check_projectile_collisions world =
  Hashtbl.iter (fun projectile_id projectile ->
    (* Skip if the projectile is already marked for deletion *)
    if projectile.lifetime > 0.0 then begin
      match Hashtbl.find_opt world.state.collider_store projectile_id with
      | Some projectile_collider ->
          check_collision_for_projectile world projectile_id projectile projectile_collider
      | None -> ()
    end
  ) world.state.projectile_store

let update_projectile_lifetime world entity projectile =
  (* Update lifetime *)
  projectile.lifetime <- projectile.lifetime -. world.dt *. 0.001;
  
  (* Check if lifetime expired *)
  if projectile.lifetime <= 0.0 then
  (
    Gfx.debug "projectile lifetime ended.\n";
    remove_entity world entity
  )



let update world =
  (* First update projectile lifetimes and check for offscreen projectiles *)
  Hashtbl.iter (fun entity projectile ->
    update_projectile_lifetime world entity projectile
  ) world.state.projectile_store;
  
  (* Then check for collisions with characters *)
  check_projectile_collisions world
