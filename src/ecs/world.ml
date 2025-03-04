open Types


module State = struct
  type t = {
    (* Component stores *)
    position_store : (Entity.t, Position.t) Hashtbl.t;
    movable_store : (Entity.t, Movable.t) Hashtbl.t;
    drawable_store : (Entity.t, Drawable.t) Hashtbl.t;
    controllable_store : (Entity.t, Controllable.t) Hashtbl.t;
    collider_store : (Entity.t, Collider.t) Hashtbl.t;
  }
  
  let create () = {
    position_store = Hashtbl.create 64;
    movable_store = Hashtbl.create 64;
    controllable_store = Hashtbl.create 64;
    drawable_store = Hashtbl.create 64;
    collider_store = Hashtbl.create 64;
  }
end

type t = {
  (* meta *)
  window : Gfx.window;
  window_surface : Gfx.surface;
  ctx : Gfx.context;
  key_names : (string, string) Hashtbl.t;
  mutable should_stop : bool;
  mutable exit_message : string;
  
  (* Input state *)
  keypresses : (string, bool) Hashtbl.t;
  
  (* Time tracking *)
  mutable dt : float;
  mutable last_frame_time : float;
  
  (* game data *)
  resources : Resources.t;
  state : State.t;

  (* Debug *)
  debug_hitboxes : bool;
}

let setup_keys k_names keys = 
  Hashtbl.add k_names "left" keys.(0);
  Hashtbl.add k_names "right" keys.(1);
  Hashtbl.add k_names "up" keys.(2);
  Hashtbl.add k_names "down" keys.(3);
  Hashtbl.add k_names "s" "s";
  Hashtbl.add k_names "z" "z"

let create window keys = 
  let context = Gfx.get_context window in
  let surface = Gfx.get_surface window in
  let k_names = Hashtbl.create 32 in
  setup_keys k_names keys;
  
 
  
  {
    window;
    window_surface = surface;
    ctx = context;
    key_names = k_names;
    
    keypresses = Hashtbl.create 10;
    dt = 0.0;
    last_frame_time = 0.0;
    should_stop = false;
    exit_message = "Still running.. If this is the exit message something very bad happened.";
    
    resources = Resources.create ();
    state = State.create ();

    debug_hitboxes = true;
  }


let rec handle_events world =
  match Gfx.poll_event () with
  | NoEvent -> ();
  | KeyDown key -> 
      Hashtbl.replace world.keypresses key true;
      handle_events world
  | KeyUp key -> 
      Hashtbl.add world.keypresses key false;
      handle_events world
  | _ -> handle_events world

