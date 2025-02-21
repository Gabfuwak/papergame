module type S = sig
  type t
  (**  the type of values accepted by the system *)

  val init : float -> unit
  (* initializes the system. The float argument is the current time in nanoseconds. *)

  val update : float -> unit
  (* updates the system. The float argument is the current time in nanoseconds *)

  val register : t -> unit
  (* register an entity for this system *)

  val unregister : t -> unit
  (* remove an entity from this system *)
end
module Make :
  functor
    (T : sig
       type t
       val id : t -> (module Entity.ID)
       val init : float -> unit
       val update : float -> t Seq.t -> unit
     end)
    -> S with type t = T.t

val init_all : float -> unit
val update_all : float -> unit
