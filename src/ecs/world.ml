open Types


module State = struct
  type t = {
    (* Component stores *)
    position_store : (Entity.t, Position.t) Hashtbl.t;
    movable_store : (Entity.t, Movable.t) Hashtbl.t;
    drawable_store : (Entity.t, Drawable.t) Hashtbl.t;
  }
  
  let create () = {
    position_store = Hashtbl.create 64;
    movable_store = Hashtbl.create 64;
    drawable_store = Hashtbl.create 64;
  }
end

type t = {
  (* meta *)
  window : Gfx.window;
  window_surface : Gfx.surface;
  ctx : Gfx.context;
  key_names : (string, string) Hashtbl.t;
  
  (* Input state *)
  keypresses : (string, bool) Hashtbl.t;
  
  (* Time tracking *)
  mutable time_acc : float;
  mutable last_frame_time : float;
  mutable should_stop : bool;
  
  (* game data *)
  resources : Resources.t;
  state : State.t;
}

let create window = 
  let context = Gfx.get_context window in
  let surface = Gfx.get_surface window in
  
  {
    window;
    window_surface = surface;
    ctx = context;
    key_names = Hashtbl.create 10;
    
    keypresses = Hashtbl.create 10;
    time_acc = 0.0;
    last_frame_time = 0.0;
    should_stop = false;
    
    resources = Resources.create ();
    state = State.create ();
  }
