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
    | Attacking of { attack_type: int; frame: int }
    | Hit of { stun_frames: int; current_frame: int }
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
  }

let get_animation_key char_name state = 
  match state with
  | Idle -> "characters/" ^ char_name ^ "/idle"
  | Walking -> "characters/" ^ char_name ^ "/walk"
  | Running -> "characters/" ^ char_name ^ "/run"
  | Attacking { attack_type = 1; _ } -> "characters/" ^ char_name ^ "/attack_forward"
  | Attacking { attack_type = 2; _ } -> "characters/" ^ char_name ^ "/attack_up"
  | Attacking _ -> failwith "unreachable" (*to make the pattern exhaustive*)
  | Hit _ -> "characters/" ^ char_name ^ "/hit"
  | JumpPrep -> "characters/" ^ char_name ^ "/jump/prep"
  | Jumping -> "characters/" ^ char_name ^ "/jump/jumping" 
  | JumpTop -> "characters/" ^ char_name ^ "/jump/top" 
  | Falling -> "characters/" ^ char_name ^ "/jump/falling" 
  | JumpRecall -> "characters/" ^ char_name ^ "/jump/recall" 
  | Dead -> "characters/" ^ char_name ^ "/dead"
end
