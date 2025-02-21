(* Main spécifique à Javascript *)
let debug = Gfx.open_formatter "console" 
let () = Gfx.set_debug_formatter debug
let () = Game.run [| "ArrowLeft"; "ArrowRight"; "ArrowUp"; "ArrowDown" |]
