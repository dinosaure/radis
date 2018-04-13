(*****************************************************************************)
(*  Copyright (C) 2018 Romain Calascibetta                                   *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation.                               *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU General Public License for more details.                             *)
(*                                                                           *)
(*  You should have received a copy of the GNU General Public License        *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

(** The Radix tree. *)

module type KEY =
sig
  type t

  val equal: t -> t -> bool
  val get: t -> int -> char
  val length: t -> int
end

(** A concrete iterable structure. *)
type 'a sequence = ('a -> unit) -> unit

(** A Radix tree is a optimized container to bind a [Key.t] with a
    value. *)
module type S =
sig
  type key
  (** The key *)

  type 'a t
  (** The radix-tree. *)

  val empty: 'a t
  (** The empty radix-tree. *)

  val is_empty: 'a t -> bool
  (** Test whether is radix-tree is empty or not. *)

  val add: key -> 'a -> 'a t -> 'a t
  (** [bind t k v] returns a radix-tree containing the same binding as
      [t], plus a binding [k] to [v]. *)

  val remove: key -> 'a t -> 'a t

  val find: key -> 'a t -> 'a

  val find_opt: key -> 'a t -> 'a option
  (** [lookup t k] returns the current binding of [k] in [t] or
      returns [None] if no such binding exists. *)

  val mem: key -> 'a t -> bool
  (** [mem t k] checks if at least we have one binding with the key
      [k]. *)

  val fold: (key -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  (** [fold f a t] computes [(f kN dN (f k1 d1 a))], where [k1 .. kN]
      are the keys of all bindings in [t], and [d1 .. dN] are the
      associated data. *)

  val iter: (key -> 'a -> unit) -> 'a t -> unit
  (** [iter f t] applies [f] to all bindings in radix-tree [t]. [f]
      receives a pair which contains the key and the associated
      value. *)

  val to_sequence: 'a t -> (key * 'a) sequence
  (** [to_sequence t] makes a abstract representation of the
      radix-tree. *)

  val to_list: 'a t -> (key * 'a) list
  (** [to_list t] makes an associated list from the radix-tree. *)

  val pp: key Fmt.t -> 'a Fmt.t -> 'a t Fmt.t
  (** A pretty-printer for the radix-tree. *)
end

module Make (Key: KEY): S with type key = Key.t
