open Vector

module Character = struct
  type character_stats = {
    air_control   : float;
    jump_force    : float;
    running_speed : float;
  }

  type state_type = 
    | Idle
    | Walking
    | Running
    | Attacking of { attack_type: int}
    | Hit of { stun_time: float; remaining_time: float; hit_vector: Vector.t }
    | JumpPrep
    | Jumping
    | JumpTop
    | Falling
    | JumpRecall
    | Dead

  type t = {
    mutable stats: character_stats;
    mutable current_state: state_type;
    mutable previous_state: state_type;
    mutable time_in_state: float;
    mutable facing_right: bool;  (* true = facing right, false = facing left *)
    mutable is_grounded: bool;
    mutable pending_hit: (Vector.t * float) option;
    mutable char_name: string;
    mutable variant: string option;
  }

let get_animation_key char state = 
  let v = 
    match char.variant with
    | None -> ""
    | Some s -> "/" ^ s 
  in
  match state with
  | Idle -> "characters/" ^ char.char_name ^ v ^ "/idle"
  | Walking -> "characters/" ^ char.char_name ^ v ^ "/walk"
  | Running -> "characters/" ^ char.char_name ^ v ^ "/run"
  | Attacking { attack_type = 0 } -> "characters/" ^ char.char_name ^ v ^ "/attack_forward"
  | Attacking { attack_type = 1 } -> "characters/" ^ char.char_name ^ v ^ "/attack_up"
  | Attacking _ -> failwith "unreachable" (*to make the pattern exhaustive*)
  (* | Hit _ -> "characters/" ^ char_name ^ v ^ "/hit" *)
  | Hit _ -> "characters/" ^ char.char_name ^ v ^ "/jump/falling" (*placeholder*)
  | JumpPrep -> "characters/" ^ char.char_name ^ v ^ "/jump/prep"
  | Jumping -> "characters/" ^ char.char_name ^ v ^ "/jump/jumping" 
  | JumpTop -> "characters/" ^ char.char_name ^ v ^ "/jump/top" 
  | Falling -> "characters/" ^ char.char_name ^ v ^ "/jump/falling" 
  | JumpRecall -> "characters/" ^ char.char_name ^ v ^ "/jump/recall" 
  | Dead -> "characters/" ^ char.char_name ^ v ^ "/dead"
end
