type t = {
  mutable x : float;
  mutable y : float;
}

let create x y = { x; y }

let zero = { x = 0.0; y = 0.0 }

let add v1 v2 = {
  x = v1.x +. v2.x;
  y = v1.y +. v2.y;
}

let sub v1 v2 = {
  x = v1.x -. v2.x;
  y = v1.y -. v2.y;
}

let scale v scalar = {
  x = v.x *. scalar;
  y = v.y *. scalar;
}

let dot v1 v2 =
  v1.x *. v2.x +. v1.y *. v2.y

let length_squared v =
  v.x *. v.x +. v.y *. v.y

let length v =
  sqrt (length_squared v)

let normalize v =
  let len = length v in
  if len > 0.0 then
    { x = v.x /. len; y = v.y /. len }
  else
    zero

let distance_squared v1 v2 =
  length_squared (sub v2 v1)

let distance v1 v2 =
  sqrt (distance_squared v1 v2)

let to_string v =
  Printf.sprintf "(%f, %f)" v.x v.y
