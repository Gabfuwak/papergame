open Types
open World  
open Resources
open Entity_creator



let update world elsapsed =  
  (* Seems unnecessary to create a whole system for just that... May change later when we add combos and stuff *)
  World.handle_events world;
  (* time tracking *)
  world.dt <- if world.last_frame_time = 0.0 then 0.0 else elsapsed -. world.last_frame_time;
  world.last_frame_time <- elsapsed;
  Control_system.update world;
  Movement_system.update world;
  Render_system.update world;
  (*world.resources.time_acc <- world.resources.time_acc +. world.dt;
  if world.resources.time_acc >= 1000.0 then begin
    world.resources.curr_texture <- (world.resources.curr_texture + 1) mod 3;
    world.resources.time_acc <- world.resources.time_acc -. 1000.0;
    Printf.printf "switching color to %d\n" world.resources.curr_texture;
    Printf.printf "Test: %s\n" world.resources.test;
  end;
  *)
  

  if world.should_stop then
    Some world.exit_message
  else
    None



let run keys =
  let window = Gfx.create "game_canvas:800x600:" in

  let world = World.create window keys in

  let player_id = create_player world 100.0 100.0 in
   
  let tileset_res = Gfx.load_file "resources/files/tile_set.txt" in
  Gfx.main_loop 
    (fun dt -> Gfx.get_resource_opt tileset_res)
    (fun tileset -> 
      Resources.load_tileset world.resources world.ctx tileset;
      Gfx.main_loop (fun dt -> update world dt) (fun exit_message -> 
        Gfx.debug "Game ended: %s\n" exit_message
      )
    )
