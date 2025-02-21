type id = int
module type ID =
sig
  val id : id
end

let id = ref (-1)
let pp fmt (module Id : ID) =
  Format.fprintf fmt "<%d>" Id.id
module Id ()=
struct
  let id = incr id; !id
end

let hash_ n =
  let n = ((n lsr 16) lxor n) * 0x45d9f3b in
  let n = ((n lsr 16) lxor n) * 0x45d9f3b in
  ((n lsr 16) lxor n)

module Table =
struct
  type 'b t = {
    mutable size : int;
    mutable mask : int;
    mutable keys : int array;
    mutable values : 'b array;
  }

  module Array = struct
    include Array
    let unsafe_get t i =
      try
        t.(i)
      with _ -> failwith (Format.sprintf "ACCESS TO INDEX: %d\n%!" i)
    let unsafe_set t i v =
      try
        t.(i) <- v
      with _ -> failwith (Format.sprintf "ACCESS TO INDEX: %d\n%!" i)
  end
  let create _ = {
    size = 0;
    mask = 15;
    keys = Array.make 16 (-1);
    values = [| |]
  }

  let clear t =
    Array.fill t.keys 0 (t.mask+1) (-1);
    t.size <- 0

  let reset t =
    t.size <- 0;
    t.mask <- 15;
    t.keys <- Array.make 16 (-1);
    t.values <- [| |]

  let next idx mask = (idx + 1) land mask
  let length t = t.size

  let rec mem_entry keys idx k mask =
    let key =  Array.unsafe_get keys idx in
    if key == -1 then false
    else key == k || mem_entry keys (next idx mask) k mask

  let mem t (module E: ID) =
    let e = E.id in mem_entry t.keys ((hash_ e) land t.mask) e t.mask

  let rec find_entry keys values idx k mask =
    let key =  Array.unsafe_get keys idx in
    if key == -1 then raise Not_found
    else if key == k then Array.unsafe_get values idx
    else find_entry keys values (next idx mask) k mask

  let find t (module E : ID) =
    let e = E.id in find_entry t.keys t.values ((hash_ e) land t.mask) e t.mask

  let find_opt t e = try Some (find t e) with Not_found -> None

  let rec add_entry keys values idx k v mask reuse =
    let key = Array.unsafe_get keys idx in
    if key = -1 then begin
      let idx = if reuse >= 0 then reuse else idx in
      Array.unsafe_set keys idx k;
      Array.unsafe_set values idx v;
      1
    end else if k == key then begin
      Array.unsafe_set values idx v;
      0
    end else
      let reuse = if reuse < 0 && key == -2 then idx else reuse in
      add_entry keys values (next idx mask) k v mask reuse

  let realloc t len nsize =
    let nkeys = Array.make nsize (-1) in
    let nvalues = Array.make nsize t.values.(0) in
    let nmask = nsize - 1 in
    for i = 0 to len - 1 do
      let key = Array.unsafe_get t.keys i in
      if key >= 0 then
        ignore (add_entry nkeys nvalues ((hash_ key) land nmask) key
                  (Array.unsafe_get t.values i) nmask (-1))
    done;
    t.keys <- nkeys;
    t.values <- nvalues;
    t.mask <- nmask

  let m3d4 n = (((n lsl 1)) + n) lsr 2

  let add t (module E : ID) v =
    if Array.length t.values = 0 then t.values <- Array.make (t.mask + 1) v;
    let cap = t.mask + 1 in
    if t.size > m3d4 cap then realloc t cap (cap lsl 1);
    let key = E.id in
    t.size <- t.size + add_entry t.keys t.values ((hash_ key) land t.mask) key v t.mask (-1)

  let replace = add
  let iter f t =
    for i = 0 to Array.length t.keys - 1 do
      let key = Array.unsafe_get t.keys i in
      if key >= 0 then
        f key (Array.unsafe_get t.values i)
    done
  let fold f t init =
    let acc = ref init in
    iter (fun k v -> acc := f k v !acc) t;
    !acc

  let rec find_absent keys idx mask =
    let key = Array.unsafe_get keys idx in
    if key == -1 then idx
    else if key < 0 then find_absent keys (next idx mask) mask
    else -1

  let rec clear_until_absent keys idx mask idx' =
    if idx != idx' then begin
      assert (Array.unsafe_get keys idx == -2);
      Array.unsafe_set keys idx (-1);
      clear_until_absent keys (next idx mask) mask idx'
    end

  let rec remove_entry keys idx k mask =
    let key = Array.unsafe_get keys idx in
    if key == -1 then 0
    else if key == k then
      let idx' = find_absent keys (next idx mask) mask in
      let () = if idx' >= 0 then begin
          clear_until_absent keys (next idx mask) mask idx';
          Array.unsafe_set keys idx (-1)
        end else Array.unsafe_set keys idx (-2)
      in 1
    else remove_entry keys (next idx mask) k mask

  let remove t (module E : ID) =
    if t.size > 0 then begin
      let e = E.id in
      t.size <- t.size - remove_entry t.keys ((hash_ e) land t.mask) e t.mask;
      let cap = t.mask + 1 in
      if t.size < (cap lsr 2) && cap > 16 then realloc t cap (cap lsr 1);
    end

  let to_seq_gen f t =
    let rec loop idx len () =
      if idx == len then Seq.Nil
      else
        let key = Array.unsafe_get t.keys idx in
        if key < 0 then loop (idx+1) len ()
        else Seq.Cons(f key (Array.unsafe_get t.values idx), (loop (idx+1) len))
    in
    loop 0 (t.mask+1)

  let to_seq_keys t = to_seq_gen (fun k _ -> k) t
  let to_seq_values t = to_seq_gen (fun _ v -> v) t
  let to_seq t = to_seq_gen (fun k v -> (k, v)) t

end

let finalizers = (Table.create 16)

let register  e (f: unit-> unit)  =
  try
    let g = Table.find finalizers e in
    Table.replace finalizers e (fun () -> f (); g())
  with Not_found -> Table.add finalizers e f

let delete e =
  try
    let f = Table.find finalizers e in
    Table.remove finalizers e;
    f ()
  with
    Not_found -> ()
