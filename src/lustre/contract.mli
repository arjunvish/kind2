(* This file is part of the Kind 2 model checker.

   Copyright (c) 2015 by the Board of Trustees of the University of Iowa

   Licensed under the Apache License, Version 2.0 (the "License"); you
   may not use this file except in compliance with the License.  You
   may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0 

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
   implied. See the License for the specific language governing
   permissions and limitations under the License. 

*)


(** Wraps a state variable for use in a contract. *)
type svar = {
  (** Position of the original svar. *)
  pos: Lib.position ;
  (** Number given to it at parse time.

  If this svar is an assumption / a guarantee, it means it's the [num]
  assumption / guarantee in the contract it's from.

  If this svar is a require / an ensure, it means it's the [num] require
  / ensure of the mode it's from. *)
  num: int ;
  (** Actual state variable. *)
  svar: StateVar.t ;
}
(** Creates a [svar]. *)
val mk_svar : Lib.position -> int -> StateVar.t -> svar

(** Type of modes. *)
type mode = {
  (** Name of the mode. *)
  name: LustreIdent.t ;
  (** Position of the mode. *)
  pos: Lib.position ;
  (** Requires of the mode. *)
  requires: svar list ;
  (** Ensures of the mode. *)
  ensures: svar list ;
}
(** Creates a [mode]. *)
val mk_mode: LustreIdent.t -> Lib.position -> svar list -> svar list -> mode

(** Type of contracts. *)
type t = {
  (** Assumptions of the contract. *)
  assumes: svar list ;
  (** Guarantees of the contract. *)
  guarantees: svar list ;
  (** Modes of the contract. *)
  modes: mode list ;
}
(* = {
  (** Assumptions of the contract. *)
  assumes: svar list ;
  (** Guarantees of the contract. *)
  guarantees: svar list ;
  (** Modes of the contract. *)
  modes: mode list ;
  (** Calls of the contract.

  List of [(id, map, t)] where
  * [id] is the name of the contract called,
  * [map] is the format to actual paramater binding of the call,
  * [t] is the contract called. *)
  calls: (LustreIdent.t * (StateVar.t * LustreExpr.t) list * t) list ;
} *)

(** Creates an empty contract. *)
val empty: unit -> t

(** Creates a new contract from a set of assumes, a set of guarantess, and a
list of modes. *)
val mk: svar list -> svar list -> mode list -> t

(** Adds assumes and guarantees to a contract. *)
val add_ass_gua: t -> svar list -> svar list -> t

(** Adds modes to a contract. *)
val add_modes: t -> mode list -> t

val svars_of: t -> StateVar.StateVarSet.t

(** Pretty prints a svar wrapper. *)
val pp_print_svar: Format.formatter -> svar -> unit

(** Pretty prints a mode. *)
val pp_print_mode: bool -> Format.formatter -> mode -> unit

(** Pretty prints a contract. *)
val pp_print_contract: bool -> Format.formatter -> t -> unit
      
(* 
   Local Variables:
   compile-command: "make -k -C .."
   indent-tabs-mode: nil
   End: 
*)
  
