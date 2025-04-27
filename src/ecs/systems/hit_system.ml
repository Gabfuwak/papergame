open World
open State.Character

let update world =
  Hashtbl.iter (fun entity character ->
    match character.pending_hit with
    | Some (hit_vector, stun_time) ->
        let damage = Vector.length hit_vector *. 0.05 in
        character.health_points <- max 0.0 (character.health_points -. damage);
        
        
    | None -> 
        (match character.current_state with
        | Hit { remaining_time; _ } ->()
            (* Apply visual effects like flashing *)
            (* Could set a flag in character for the renderer *)
            
            (* Handle invulnerability frames here if needed *)
            
        | _ -> ())
        
  ) world.state.character_store
