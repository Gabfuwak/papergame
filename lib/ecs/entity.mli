type id [@@ immediate]
module type ID =
sig
  val id : id
end
module Id () : ID

val pp : Format.formatter -> (module ID) -> unit

val delete : (module ID) -> unit
(** [delete e] deletes an entity. Although enties do not store anything,
    deleting them unregisters them from the various sytems and components they
    are registered against, freeing the associated resources.
    @see {register}
*)

val register : (module ID) -> (unit -> unit) -> unit
(** [register id finalizer] is used internally by {Component} and {System} to
    register finalizers for components and systems. This allows one to simply
    call delete on an entity, it will be automatically removed from the systems
    its registered against, and will remove all of its components.
*)



(** A module implementing a hash table whose keys are entities (tagged with the
      same type). This module implements a subset of the {Hashtbl.S} signature.
      The implementation is faster for our use case and more compact in memory.
*)
module Table : sig
  type 'a t

  val create : int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val length : 'a t -> int
  val add : 'a t -> (module ID) -> 'a -> unit
  val replace : 'a t -> (module ID) -> 'a -> unit
  val mem : 'a t -> (module ID) -> bool
  val find : 'a t -> (module ID) -> 'a
  val find_opt : 'a t -> (module ID) -> 'a option
  val remove : 'a t -> (module ID) -> unit
  val iter : (id -> 'a -> unit) -> 'a t -> unit
  val fold : (id -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val to_seq : 'a t -> (id * 'a) Seq.t
  val to_seq_keys : 'a t -> id Seq.t
  val to_seq_values : 'a t -> 'a Seq.t

end