open Types
open World  
open Resources
open Entity_creator

let setup world = 
  let _ = create_player world 100.0 100.0 "characters_ink_master_walk_cycle" in
  ()


let update world elapsed =  
  (* Seems unnecessary to create a whole system for just that... May change later when we add combos and stuff *)
  World.handle_events world;
  
  (* time tracking *)
  world.dt <- if world.last_frame_time = 0.0 then 0.0 else elapsed -. world.last_frame_time;
  world.last_frame_time <- elapsed;
  
  (* systems *)
  Control_system.update world;
  Movement_system.update world;
  Render_system.update world;
  
  if world.should_stop then
    Some world.exit_message
  else
    None

let run keys =
  let window = Gfx.create "game_canvas:800x600:" in
  let world = World.create window keys in

  (* This is basically just doing Gfx.load_file and Gfx.load_image and putting it in a tuple *)
  let resource_handles = Resources.init_atlas_loading world.ctx in
  
  Gfx.debug "Entering resource loading loop...\n";
  Gfx.main_loop
    (fun elapsed -> 
      let result = Resources.are_resources_ready resource_handles in
      if result = None then Gfx.debug "Waiting for resources... (%.2f ms)\n" elapsed;
      result
    )
    (fun (atlas, metadata) -> 
      Gfx.debug "Resources are ready! Processing atlas...\n";
      
      (* Process the atlas *)
      Resources.process_atlas world.resources world.ctx atlas metadata;
      
      Gfx.debug "Atlas processing complete! Starting game loop...\n";

      (* Setup the game *)
      setup world;

      (* Start game loop *)
      Gfx.main_loop 
        (fun elapsed -> 
          let result = update world elapsed in
          if world.dt > 20.0 then Gfx.debug "Slow frame: %.2f ms\n" world.dt;
          result
        )
        (fun exit_message -> Gfx.debug "Game ended: %s\n" exit_message)
    )
