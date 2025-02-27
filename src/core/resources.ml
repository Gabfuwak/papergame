open Types
type t = {

  textures : (string, Types.texture) Hashtbl.t;
  
  files : (string, string) Hashtbl.t;
  
  fonts : (string, Gfx.font) Hashtbl.t;

  (* This is just debug, will be removed *)
  mutable textures_array : string array;
  mutable curr_texture : int;
  mutable test : string;
  mutable time_acc : float;

}

let create () = {
  textures = Hashtbl.create 10;
  files = Hashtbl.create 10;
  fonts = Hashtbl.create 10;
  textures_array = [||];
  curr_texture = 0;
  test = "Not initialized";
  time_acc = 0.0;
}

let parse_tileset tileset =
  (* Split the tileset string into lines to get image filenames *)
  let filenames = String.split_on_char '\n' tileset in
  (* Filter out any empty lines *)
  List.filter (fun s -> String.length s > 0) filenames

let process_tileset_resources res_record resources =
  (* Store each image in the hashtable by its filename *)
  List.iter 
        (fun (filename, res) -> 
          let surface = Gfx.get_resource res in
          let texture = Image surface in
          Hashtbl.add res_record.textures filename texture) 
        resources;
  (* Create an array of the loaded textures for cycling display *)
  let filenames = Array.of_list (
    List.map 
      (fun (filename, _) -> filename) 
      resources
  ) in
  
  (* Update the textures array with our loaded images *)
  res_record.textures_array <- filenames;
  res_record.test <- "Loaded " ^ (string_of_int (List.length resources)) ^ " images"; 
  ()

let load_tileset res_record ctx tileset =
  let filenames = parse_tileset tileset in
  
  (* Start loading all images at once *)
  let image_resources = List.map 
    (fun filename -> 
      (filename, Gfx.load_image ctx ("resources/images/" ^ filename))) 
    filenames in
  
  (* Check if all resources are ready *)
  let rec check_all_ready resources =
    List.for_all (fun (_, res) -> Gfx.resource_ready res) resources
  in
  
  (* Set up the main loop to wait until all resources are ready *)
  Gfx.main_loop
    (fun _dt -> 
      if check_all_ready image_resources then
        Some image_resources
      else
        None)
    (fun resources ->
      process_tileset_resources res_record resources;
    )
