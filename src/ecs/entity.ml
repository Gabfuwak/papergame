type t = int

let next_id = ref 0

let create () = 
  let id = !next_id in
  incr next_id;
  id
