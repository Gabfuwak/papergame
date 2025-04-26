open Types
open Vector
open State.Character
open World
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
    let x_correction = if character.facing_right then 175.0 else -175.0 in
    position.Position.pos <- Vector.add position.Position.pos (Vector.create x_correction 10.0)
  | _ -> 
    (* Default case for other characters/attacks *)
    ()

(* Create a paint projectile *)
let create_paint_projectile world entity character position =
  (* Calculate spawn position relative to character *)
  let dir_multiplier = if character.facing_right then 1.0 else -1.0 in
  let spawn_x = position.Position.pos.x +. (50.0 *. dir_multiplier) in
  let spawn_y = position.Position.pos.y -. 150.0 in
  
  (* Get the variant for the projectile texture *)
  let variant = 
    match character.variant with
    | None -> ""
    | Some v -> "/" ^ v
  in
  
  (* Get animation key for the projectile *)
  let animation_key = "characters/color_witch" ^ variant ^ "/paint" in
  
  (* Create the projectile entity *)
  let projectile_id = create_projectile world spawn_x spawn_y animation_key dir_multiplier entity in

  projectile_id
  
(* Function to handle attack execution *)
let execute_attack world entity character position attack_type =
  (* First apply any position corrections *)
  apply_position_correction character position attack_type;
  
  (* Then create projectiles or handle other attack effects *)
  match character.char_name, attack_type with
  | "color_witch", 0 -> 
    (* Forward paint attack - create projectile *)
    ignore @@ create_paint_projectile world entity character position;
    true
  | "color_witch", 1 ->
    (* Up paint attack - possibly different projectile behavior *)
    (* For now, use the same projectile but with upward trajectory *)
    ignore @@ create_paint_projectile world entity character position;
    true
  | _ -> 
    (* Other characters or attack types - no projectile created *)
    false
