open Types
open World  
open Resources
open Entity_creator

let setup world = 
  let width, height = Gfx.get_context_logical_size world.ctx in
  let width_f = float_of_int width in
  let height_f = float_of_int height in
  let wall_thickness = 10.0 in

  (* Top *)
  ignore @@ create_wall world 0.0 0.0 width_f wall_thickness;
  (* Bottom *)
  ignore @@ create_wall world 0.0 (height_f -. wall_thickness) width_f wall_thickness;
  (* Left *)
  ignore @@ create_wall world 0.0 0.0 10.0 height_f;
  (* Right *)
  ignore @@ create_wall world (width_f -. wall_thickness) 0.0 10.0 height_f;

  let ball_id = create_ball world in
  ignore @@ create_paddle world true;
  ignore @@ create_paddle world false;

  ignore @@ create_camera world (Some ball_id) (width_f /. 2.0) (height_f /. 2.0) width_f height_f 1.0;
  ()


let update world elapsed =  
  (* Seems unnecessary to create a whole system for just that... May change later when we add combos and stuff *)
  World.handle_events world;
  
  (* time tracking *)
  world.dt <- if world.last_frame_time = 0.0 then 0.0 else elapsed -. world.last_frame_time;
  world.last_frame_time <- elapsed;
  
  (* systems *)
  Control_system.update world;
  Collision_system.update world;
  Movement_system.update world;
  Camera_system.update world;
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
