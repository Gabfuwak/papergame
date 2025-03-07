open Types

type t = {
  textures : (string, Types.texture) Hashtbl.t;
  files : (string, string) Hashtbl.t;
  fonts : (string, Gfx.font) Hashtbl.t;
}

let create () = {
  textures = Hashtbl.create 10;
  files = Hashtbl.create 10;
  fonts = Hashtbl.create 10;
}

let parse_atlas_metadata content =
  let lines = String.split_on_char '\n' content in
  let atlas_items = Hashtbl.create (List.length lines) in
  
  List.iter (fun line ->
    if String.length line > 0 then
      let parts = String.split_on_char ':' line in
      match parts with
      | "tile" :: name :: x :: y :: width :: height :: [] ->
          let region = {
            x = int_of_string x;
            y = int_of_string y;
            width = int_of_string width;
            height = int_of_string height;
          } in
          Hashtbl.add atlas_items name (StaticTile region)
      | "anim" :: name :: x :: y :: width :: height :: frames :: framerate :: [] ->
          let region = {
            x = int_of_string x;
            y = int_of_string y;
            width = int_of_string width;
            height = int_of_string height;
          } in
          let anim = AnimationItem {
            base_region = region;
            frames = int_of_string frames;
            framerate = float_of_string framerate;
          } in
          Hashtbl.add atlas_items name anim
      | _ -> 
          Gfx.debug "Invalid atlas metadata line: %s\n" line
  ) lines;
  
  atlas_items

let extract_region ctx atlas region =
  let surface = Gfx.create_surface ctx region.width region.height in
  Gfx.blit_full ctx surface atlas 
    region.x region.y region.width region.height  (* Source region *)
    0 0 region.width region.height;  (* Destination region (full surface) *)
  surface

let process_atlas resources ctx atlas metadata =
  let atlas_items = parse_atlas_metadata metadata in
  
  Hashtbl.iter (fun name item ->
    match item with
    | StaticTile region ->
        let surface = extract_region ctx atlas region in
        Gfx.debug "Added StaticTile %s\n" name;
        Hashtbl.add resources.textures name (Image surface)
        
    | AnimationItem { base_region; frames; framerate } ->
        let frame_surfaces = Array.init frames (fun i ->
          let frame_region = {
            x = base_region.x + i * base_region.width;
            y = base_region.y;
            width = base_region.width;
            height = base_region.height;
          } in
          extract_region ctx atlas frame_region
        ) in
        
        let animation = Animation {
          frames = frame_surfaces;
          framerate;
          current_frame = 0;
          accumulated_time = 0.0;
        } in
        
        Gfx.debug "Added animation %s\n" name;
        Hashtbl.add resources.textures name animation
  ) atlas_items

let init_atlas_loading ctx =
  let atlas_res = Gfx.load_image ctx "resources/images/tileset.png" in
  let metadata_res = Gfx.load_file "resources/files/tileset.txt" in
  (atlas_res, metadata_res)

let are_resources_ready (atlas_res, metadata_res) =
  if Gfx.resource_ready atlas_res && Gfx.resource_ready metadata_res then
    let atlas = Gfx.get_resource atlas_res in
    let metadata = Gfx.get_resource metadata_res in
    Some (atlas, metadata)
  else
    None
