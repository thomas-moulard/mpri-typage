(** This module implements a simple and efficient union/find algorithm.
    See Robert E. Tarjan, ``Efficiency of a Good But Not Linear Set
    Union Algorithm'', JACM 22(2), 1975. *)

(** The abstraction defined by this module is a set of points,
    partitioned into equivalence classes. With each equivalence class,
    a piece of information, of abstract type ['a], is associated; we
    call it a descriptor. *)
type 'a point

(** [fresh desc] creates a fresh point and returns it. It forms an
    equivalence class of its own, whose descriptor is [desc]. *)
val fresh: 'a -> 'a point

(** [find point] returns the descriptor associated with [point]'s
    equivalence class. *)
val find: 'a point -> 'a

(** [union point1 point2] merges the equivalence classes associated
    with [point1] and [point2] (which must be distinct) into a single
    class whose descriptor is that originally associated with [point2]. *)
val union: 'a point -> 'a point -> unit

(** [equivalent point1 point2] tells whether [point1] and [point2]
    belong to the same equivalence class. *)
val equivalent: 'a point -> 'a point -> bool

(** [redundant] maps all members of an equivalence class, but one, to
    [true]. *)
val redundant: 'a point -> bool

(** [change p d] updates the descriptor of [p] to [d]. *)
val change: 'a point -> 'a -> 'a  
