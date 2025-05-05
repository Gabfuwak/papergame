open Types
open Vector
open State.Character
open World
open Projectile
open Entity_creator

(* Helper for position corrections based on character and attack type *)
let apply_position_correction character position attack_type =
  match character.char_name, attack_type with
  | "color_witch", 0 -> (* forward paint attack *)
    (* No position correction needed for color_witch forward attack *)
    ()
  | "color_witch", 1 -> (* up paint attack *)
    (* Any position correction needed for up attack *)
    ()
  | "ink_master", 0 -> (* forward attack *)
    (* No correction needed *)
    ()
  | "ink_master", 1 -> (* up attack *)
    let x_correction = if character.facing_right then 175.0 else -.175.0 in
    position.Position.pos <- Vector.add position.Position.pos (Vector.create x_correction 10.0)
  | _ -> 
    (* Default case for other characters/attacks *)
    ()

let create_paint_projectile world entity character position gravity_scale =
  let dir_multiplier = if character.facing_right then 1.0 else -1.0 in
  let spawn_x = position.Position.pos.x +. (50.0 *. dir_multiplier) in
  let spawn_y = position.Position.pos.y -. 150.0 in
  
  let variant = 
    match character.variant with
    | None -> ""
    | Some v -> "/" ^ v
  in
  
  let animation_key = "characters/color_witch" ^ variant ^ "/paint" in
  
  let projectile_id = create_projectile world spawn_x spawn_y animation_key dir_multiplier entity gravity_scale in

  projectile_id
  
let execute_attack world entity character position attack_type =

  match character.char_name, attack_type with
  | "color_witch", 0 ->
    ignore @@ create_paint_projectile world entity character position 0.0;
    true
  | "color_witch", 1 ->
    let projectile_id = create_paint_projectile world entity character position 1.0 in
    let proj_opt = Hashtbl.find_opt world.state.projectile_store projectile_id in
    (match proj_opt with
    | Some proj -> 
        proj.damage <- proj.damage + 5;
    | None -> ());
    true
  | "ink_master", 0 ->
    true
  | "ink_master", 1 ->
    true
  | _ ->
    false


let handle_attack_hit world attacker_char defender_id defender_char attack_type =
  attacker_char.hit_entities <- defender_id :: attacker_char.hit_entities;

  let damage = match attacker_char.char_name, attack_type with
    | "color_witch", 0 -> 8.0  (* Color witch forward attack *)
    | "color_witch", 1 -> 12.0 (* Color witch up attack *)
    | "ink_master", 0 -> 10.0  (* Ink master forward attack *)
    | "ink_master", 1 -> 15.0  (* Ink master up attack *)
    | _, _ -> failwith "unreachable: src/ecs/utils/attacks.ml" 
  in

  defender_char.health_points <- max 0.0 (defender_char.health_points -. damage);

  Gfx.debug "Character %d hit! Health: %.1f/%.1f"
    defender_id defender_char.health_points defender_char.max_hp;

  let base_x = if attacker_char.facing_right then 400.0 else -400.0 in
  let hit_vector = match attacker_char.char_name, attack_type with
    | "color_witch", 0 -> Vector.create base_x (-200.0) 
    | "color_witch", 1 -> Vector.create (base_x *. 0.3) (-450.0)
    | "ink_master", 0 -> Vector.create base_x (-200.0)
    | "ink_master", 1 -> Vector.create (base_x *. 0.5) (-400.0)
    | _, _ -> Vector.zero
  in

  let stun_time = match attacker_char.char_name, attack_type with
    | "ink_master", 1 -> 0.7  (* Ink master up attack has longer stun *)
    | _, _ -> 0.5            (* Default stun time *)
  in
  
  defender_char.pending_hit <- Some (hit_vector, stun_time)

let handle_projectile_hit world projectile target_id target_char =
  (* Apply damage based on projectile properties *)
  let damage = float_of_int projectile.damage in
  
  target_char.health_points <- max 0.0 (target_char.health_points -. damage);

  Gfx.debug "Character %d hit by projectile! Health: %.1f/%.1f"
    target_id target_char.health_points target_char.max_hp;

  (* Calculate knockback vector based on projectile direction *)
  let hit_direction = projectile.direction in
  let base_x = if hit_direction > 0.0 then 300.0 else -300.0 in
  let hit_vector = Vector.create base_x (-200.0) in

  (* Set pending hit state on defender with appropriate stun time *)
  target_char.pending_hit <- Some (hit_vector, 0.5)
