open Types
open World
open Entity
open State
open Position
open Movable
open Controllable
open Collider
open Drawable
open Camera
open Vector

type control_scheme =
  | ZQSD_Controls
  | IJKL_Controls

let create_controls scheme =
    let controls = Hashtbl.create 10 in
    let attack_forward_control = Attack {attack_type = 0} in
    let attack_up_control = Attack {attack_type = 1} in


    match scheme with
  | ZQSD_Controls ->
      Hashtbl.add controls Up "z";
      Hashtbl.add controls Down "s";
      Hashtbl.add controls Left "q";
      Hashtbl.add controls Right "d";
      Hashtbl.add controls Jump "space";
      Hashtbl.add controls attack_forward_control "a";
      Hashtbl.add controls attack_up_control "e";
      controls
  | IJKL_Controls ->
      Hashtbl.add controls Up "i";
      Hashtbl.add controls Down "k";
      Hashtbl.add controls Left "j";
      Hashtbl.add controls Right "l";
      Hashtbl.add controls Jump "m";
      Hashtbl.add controls attack_forward_control "u";
      Hashtbl.add controls attack_up_control "o";
      controls

let create_player world x y char_name variant scheme =
  let id = Entity.create () in
  let position = { pos = Vector.create x y } in
  let movable = { velocity = Vector.create 0.0 0.0; force = Vector.create 0.0 0.0; gravity_scale = 1.0 } in
  let controls = create_controls scheme in
  let controllable = {controls = controls} in
  
  Hashtbl.add world.state.position_store id position;
  Hashtbl.add world.state.movable_store id movable;
  Hashtbl.add world.state.controllable_store id controllable;

  let black = Gfx.color 0 0 0 255 in

  let v = 
    match variant with
    | None -> ""
    | Some s -> "/" ^ s
  in

  let tex = "characters/" ^ char_name ^ v ^ "/idle" in
  
  let texture =
    match Hashtbl.find_opt world.resources.textures tex with
    | Some text -> text
    | None -> 
        Gfx.debug "Error: texture %s does not exist\n" tex;
        Color black
  in
  
  let drawable = { texture = texture } in

  Hashtbl.add world.state.drawable_store id drawable;

  let stats = {
    Character.air_control = 200.0;
    Character.running_speed = 800.0;
    Character.jump_force = 800.0;
  } in

  let character = {
    Character.stats = stats;
    Character.current_state = State.Character.Idle;
    Character.previous_state = State.Character.Idle;
    Character.time_in_state = 0.0;
    Character.facing_right = true;
    Character.is_grounded = false;
    Character.pending_hit = None;
    Character.char_name = char_name;
    Character.variant = variant;
    Character.max_hp = 100.0;
    Character.health_points = 75.0;
    Character.hit_entities = [];
  } in

  Hashtbl.add world.state.character_store id character;


  let hitboxes =
    match Hashtbl.find_opt world.resources.texture_hitboxes tex with
    | Some text -> text.(0)
    | None -> 
         [|{
            boxtype = "vulnerable";
            pos = Vector.create 0.0 0.0;
            width = 200.0;
            height = 200.0;
         }|]
  in


  let collider = {
    origin_pos = position.pos;
    boxes = hitboxes;
    weight = 100.0;
  } in

  Hashtbl.add world.state.collider_store id collider;

  
  id

let create_target_dummy world x y char_name variant =
  let id = Entity.create () in
  let position = { pos = Vector.create x y } in
  let movable = { velocity = Vector.create 0.0 0.0; force = Vector.create 0.0 0.0; gravity_scale = 1.0} in
  
  let controls = Hashtbl.create 10 in
  let controllable = {controls = controls} in
  
  Hashtbl.add world.state.position_store id position;
  Hashtbl.add world.state.movable_store id movable;
  Hashtbl.add world.state.controllable_store id controllable;
  
  let v = 
    match variant with
    | None -> ""
    | Some s -> "/" ^ s
  in

  let tex = "characters/" ^ char_name ^ v ^ "/idle" in
  let texture =
    match Hashtbl.find_opt world.resources.textures tex with
    | Some text -> text
    | None ->
        Gfx.debug "Error: texture %s does not exist\n" tex;
        Color (Gfx.color 255 0 0 255)
  in
  
  let drawable = { texture = texture } in
  Hashtbl.add world.state.drawable_store id drawable;
  
  (* Create character stats *)
  let stats = {
    Character.air_control = 200.0;
    Character.running_speed = 0.0;
    Character.jump_force = 0.0;
  } in
  
  let character = {
    Character.stats = stats;
    Character.current_state = State.Character.Idle;
    Character.previous_state = State.Character.Idle;
    Character.time_in_state = 0.0;
    Character.facing_right = true;
    Character.is_grounded = false;
    Character.pending_hit = None;
    Character.char_name = char_name;
    Character.variant = variant;
    Character.max_hp = 100.0;
    Character.health_points = 100.0;
    Character.hit_entities = [];
  } in
  
  Hashtbl.add world.state.character_store id character;
  
  let hitboxes =
    match Hashtbl.find_opt world.resources.texture_hitboxes tex with
    | Some text -> text.(0)
    | None ->
         [|{
            boxtype = "vulnerable";
            pos = Vector.create 0.0 0.0;
            width = 200.0;
            height = 200.0;
         }|]
  in
  
  let collider = {
    origin_pos = position.pos;
    boxes = hitboxes;
    weight = 100.0;
  } in
  
  Hashtbl.add world.state.collider_store id collider;
  id

let create_camera world target x y width height zoom =
  let id = Entity.create () in
  let position = { pos = Vector.create x y } in
  let camera = { 
    zoom = zoom; 
    width = width; 
    height = height;
    target = target;
    offset = Vector.create 0.0 0.0;
  } in
  
  Hashtbl.add world.state.position_store id position;
  Hashtbl.add world.state.camera_store id camera;
  
  if world.active_camera_id = None then
    world.active_camera_id <- Some id;
  
  id

let create_prop world x y prop_name =
  let id = Entity.create () in

  let tex_path = "decor/props/" ^ prop_name in

  let position = { pos = Vector.create x y } in
  Hashtbl.add world.state.position_store id position;

  let texture =
    match Hashtbl.find_opt world.resources.textures tex_path with
    | Some tex -> tex
    | None ->
        Gfx.debug "Error: Prop texture '%s' not found. Using fallback color.\n" tex_path;
        Color (Gfx.color 255 0 255 255) (* Bright pink fallback *)
  in

  let drawable = { texture = texture } in
  Hashtbl.add world.state.drawable_store id drawable;

  id

let create_platform world x y platform_type =
  let id = Entity.create () in

  let tex_path_base = "decor/platforms/" in
  let tex_path =
    match platform_type with
    | "big" -> tex_path_base ^ "big_platform"
    | "small" -> tex_path_base ^ "small_platform"
    | _ ->
        Gfx.debug "Error: Invalid platform type '%s'. Using 'small_platform' as fallback.\n" platform_type;
        tex_path_base ^ "small_platform"
  in

  let position = { pos = Vector.create x y } in
  Hashtbl.add world.state.position_store id position;

  let texture =
    match Hashtbl.find_opt world.resources.textures tex_path with
    | Some tex -> tex
    | None ->
        Gfx.debug "Error: Platform texture '%s' not found. Using fallback color.\n" tex_path;
        Color (Gfx.color 128 128 128 255)
  in
  let drawable = { texture = texture } in
  Hashtbl.add world.state.drawable_store id drawable;

  let width, height =
    match texture with
    | Image i -> Gfx.surface_size i.surface
    | Animation a ->
        if Array.length a.frames > 0 then
          Gfx.surface_size a.frames.(0)
        else
          (100, 20)
    | Color _ ->
        match platform_type with
         | "big" -> (300, 50)
         | "small" -> (150, 40)
         | _ -> (150, 40) 
  in

  let hitbox = {
    boxtype = "vulnerable"; 
    pos = Vector.create 0.0 0.0;
    width = float_of_int width;
    height = float_of_int height;
  } in
  let collider = {
    origin_pos = position.pos; 
    boxes = [| hitbox |];
    weight = Float.infinity;
  } in
  Hashtbl.add world.state.collider_store id collider;

  id


let create_wall world x y width height =
  let id = Entity.create() in
  
  let position = { pos = Vector.create x y } in
  let hitbox = {
    boxtype = "";
    pos = Vector.create 0.0 0.0;
    width = width;
    height = height;
  } in
  
  let collider = {
    origin_pos = position.pos;
    boxes = [| hitbox |];
    weight = Float.infinity;
  } in

  let black = Gfx.color 0 0 0 255 in
  let texture = Color black in
  let drawable = {
    texture = texture
  } in
  
  Hashtbl.add world.state.position_store id position;
  Hashtbl.add world.state.collider_store id collider;
  Hashtbl.add world.state.drawable_store id drawable;
  
  id


let create_projectile world x y animation_key direction source_entity gravity_scale =
  let entity = Entity.create() in
  
  (* Add position component *)
  let position = { Position.pos = Vector.create x y } in
  Hashtbl.add world.state.position_store entity position;
  
  (* Add movable component with initial velocity *)
  let speed = 1200.0 in (* Adjust projectile speed as needed *)
  let movable = { 
    velocity = Vector.create (direction *. speed) 0.0;
    force = Vector.create 0.0 0.0;
    gravity_scale = gravity_scale;
  } in
  Hashtbl.add world.state.movable_store entity movable;
  
  (* Add drawable component *)
  let texture = 
    match Hashtbl.find_opt world.resources.textures animation_key with
    | Some tex -> tex
    | None -> 
        Gfx.debug "Missing projectile texture: %s" animation_key;
        Hashtbl.find world.resources.textures "missing"
  in

  let drawable = { 
    Drawable.texture = texture;
  } in
  Hashtbl.add world.state.drawable_store entity drawable;
  
  (* Add collider component *)
  let size = 
    match texture with
    | Image i -> Gfx.surface_size i.surface
    | Animation a -> Gfx.surface_size a.frames.(0)
    | _ -> (32, 32) (* Default size *)
  in
  
  let width, height = size in
  let collider = { 
    Collider.boxes = [|{
      Collider.boxtype = "attack";
      Collider.pos = Vector.create 0.0 0.0;
      Collider.width = float_of_int width;
      Collider.height = float_of_int height;
    }|];
    Collider.weight = 0.0; (* Projectiles have no weight *)
    Collider.origin_pos = position.Position.pos;
  } in
  Hashtbl.add world.state.collider_store entity collider;
  
  let projectile = {
    Projectile.lifetime = 1.0;
    Projectile.damage = 10;    (* Base damage *)
    Projectile.source_entity = source_entity;
    Projectile.projectile_type = "paint";
    Projectile.direction = direction;
    Projectile.destroyed_on_hit = true;
  } in
  Hashtbl.add world.state.projectile_store entity projectile;
  
  (* Return the entity ID *)
  entity
