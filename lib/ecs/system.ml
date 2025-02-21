module type T =
sig
  type t
  val id : t -> (module Entity.ID)
  val init : float -> unit
  val update : float -> t Seq.t -> unit
end

module type S = sig
  type t
  (**  the type of the entitiesvalues accepted by the system *)

  val init : float -> unit
  (* initializes the system. The float argument is the current time in nanoseconds. *)

  val update : float -> unit
  (* updates the system. The float argument is the current time in nanoseconds *)

  val register : t -> unit
  (* register an entity for this system *)

  val unregister : t -> unit
  (* remove an entity from this system *)
end


let systems = Queue.create ()
let register m = Queue.add m systems

module PreMake (X : T) : S with type t = X.t =
struct
  type t = X.t
  let table = Entity.Table.create 16
  let register e =
    let eid = X.id e in
    if Entity.Table.mem table eid then
      failwith (Format.asprintf "Entity %a is already registered" Entity.pp eid)
    else
      Entity.Table.add table eid e;
    Entity.register eid (fun () -> Entity.Table.remove table eid)

  let unregister e =
    let eid = X.id e in
    Entity.Table.remove table eid
  let init dt = X.init dt
  let update dt = X.update dt (Entity.Table.to_seq_values  table)
end
module Make (X:T) : S with type t = X.t =
struct
  module M = PreMake(X)
  let () = register (module M : S)
  type t = M.t
  let init = M.init
  let update = M.update
  let register = M.register
  let unregister = M.unregister
end

let init_all dt =
  Queue.iter
    (fun m ->
       let module M = (val m : S) in
       M.init dt)
    systems

let update_all dt =
  Queue.iter
    (fun m ->
       let module M = (val m : S) in
       M.update dt)
    systems
