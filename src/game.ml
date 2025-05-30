open Types
open State.Character
open World  
open Resources
open Entity_creator

let setup world =
  let screen_w, screen_h = Gfx.get_window_size world.window in
  let aspect_ratio = (float_of_int screen_w)/.(float_of_int screen_h) in
  let height_f = 600.0 in (* Default world space height of camera view *)
  let width_f = height_f *. aspect_ratio in

  let ground_level = 850.0 in
  let platform_spacing = 800.0 in
  let platform_height_variation = 400.0 in

  ignore @@ create_platform world 0.0 ground_level "big";
  ignore @@ create_platform world 1000.0 ground_level "big"; 
  ignore @@ create_platform world 2000.0 ground_level "big";
  ignore @@ create_platform world (-.1000.0) ground_level "big";

  ignore @@ create_platform world (-.1000.0) (ground_level -. platform_height_variation) "small";
  ignore @@ create_platform world 0.0 (ground_level -. platform_height_variation *. 1.5) "small";
  ignore @@ create_platform world (500.0 +. platform_spacing *. 2.0) (ground_level -. platform_height_variation *. 0.8) "big";
  ignore @@ create_platform world (500.0 +. platform_spacing *. 3.0) (ground_level -. platform_height_variation *. 1.2) "small";



  ignore @@ create_prop world 150.0 (ground_level -. 60.0) "bush_a";
  ignore @@ create_prop world 400.0 (ground_level -. 80.0) "rock_b";
  ignore @@ create_prop world 750.0 (ground_level -. 590.0) "tree_a";
  ignore @@ create_prop world 1000.0 (ground_level -. 40.0) "flower_b";
  ignore @@ create_prop world 1300.0 (ground_level -. 65.0) "bush_c";


  let player1_start_y = ground_level -. 250.0 in
  let player2_start_y = ground_level -. 250.0 in

  let player1 = create_player world 0.0 player1_start_y "color_witch" (Some "red") ZQSD_Controls in
  let player2 = create_player world 1500.0 player2_start_y "ink_master" None IJKL_Controls in

  ignore @@ create_camera world (Some [player1; player2]) (width_f /. 2.0) (height_f /. 2.0) width_f height_f 0.8; (* Zoomed out slightly *)

  ()


let update world elapsed =  
  (* Seems unnecessary to create a whole system for just that... May change later when we add combos and stuff *)
  World.handle_events world;
  
  (* time tracking *)
  world.dt <- if world.last_frame_time = 0.0 then 0.0 else elapsed -. world.last_frame_time;
  world.last_frame_time <- elapsed;
  
  (* systems *)
  (* Control_system.update world; *)

  Projectile_system.update world;

  
  State_machine.update world;

  (* this is a dirty fix and i don't like it but whatever *)
  Hashtbl.iter (fun id character ->
    character.is_grounded <- false
  ) world.state.character_store;

  let subdivisions = 10 in
  let sub_dt = world.dt /. float_of_int subdivisions in
  
  for _ = 1 to subdivisions do
    let original_dt = world.dt in
  
    world.dt <- sub_dt;
  
    Physics_system.update world;
    Movement_system.update world;
    Collision_system.update world;
  
    world.dt <- original_dt;
  done;

  Hit_system.update world;

  Camera_system.update world;
  Render_system.update world;
  Healthbar_system.update world;
  
  Gfx.commit world.ctx;

  Gfx.debug "%!";
  
  if world.should_stop then
    Some world.exit_message
  else
    None

let run keys =
  let screen_h = 1080 in
  let screen_w = 1920 in
  let window = Gfx.create ("game_canvas:"^ string_of_int screen_w ^"x"^string_of_int screen_h^":") in
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
          if world.dt > 20.0 then(
            Gfx.debug "Slow frame: %.2f ms\n" world.dt
          )
          else(
            (*Gfx.debug "Current approx fps: %f\n" (1000.0/.world.dt)*)
          );
            result

        )
        (fun exit_message -> Gfx.debug "Game ended: %s\n" exit_message)
    )
