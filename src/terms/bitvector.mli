(** This file is part of the Kind 2 model checker.

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

(** Bit-vectors

    @author Arjun Viswanathan
*)



(** Bitvector type *)
type t


(** {Numeral to Unsigned Bitvector} *)

(** Return size 8 unsigned bitvector converted from a numeral *)
val num_to_ubv8 : Numeral.t -> t

(** Return size 16 unsigned bitvector converted from a numeral *)
val num_to_ubv16 : Numeral.t -> t

(** Return size 32 unsigned bitvector converted from a numeral *)
val num_to_ubv32 : Numeral.t -> t

(** Return size 64 unsigned bitvector converted from a numeral *)
val num_to_ubv64 : Numeral.t -> t


(** {Numeral to Signed Bitvector} *)

(** Return size 8 signed bitvector converted from a numeral *)
val num_to_bv8 : Numeral.t -> t

(** Return size 16 signed bitvector converted from a numeral *)
val num_to_bv16 : Numeral.t -> t

(** Return size 32 signed bitvector converted from a numeral *)
val num_to_bv32 : Numeral.t -> t

(** Return size 64 signed bitvector converted from a numeral *)
val num_to_bv64 : Numeral.t -> t


(** {Bitvector to Numeral} *)

(** Return numeral converted from a bitvector *)
val bv_to_num : t -> Numeral.t


(** {Constants} *)

(** Return bitvector zero of given size *)
val zero : int -> t


(** {Arithmetic Operations} *)

(** Addition *)
val add : t -> t -> t
 
(** Subtraction *)
val sub : t -> t -> t
 
(** Multiplication *)
val mul : t -> t -> t
 
(** Division *)
(** Raises Division_by_zero exception if second argument is zero *)
val div : t -> t -> t
 
(** Remainder *)
(** Raises Division_by_zero exception if second argument is zero *)
val rem : t -> t -> t
 
(** Negation *)
val neg : t -> t


(** {Logical Operations} *)

(** Bitwise and *)
val logand : t -> t -> t
 
(** Bitwise or *)
val logor : t -> t -> t
 
(** Bitwise not *)
val lognot : t -> t


(** {Conversion Operations} *)

(** uintN -> uint8 *)
val to_uint8 : t -> t 
 
(** uintN -> uint16 *)
val to_uint16 : t -> t 
 
(** uintN -> uint32 *)
val to_uint32 : t -> t 
 
(** uintN -> uint64 *)
val to_uint64 : t -> t 
 
(** intN -> int8 *)
val to_int8 : t -> t
 
(** intN -> int16 *)
val to_int16 : t -> t
 
(** intN -> int32 *)
val to_int32 : t -> t

(** intN -> int64 *)
val to_int64 : t -> t


(** {Shift Operators} *)
(** Shift left *)
val bvshl : t -> t -> t
 
(** Shift right *)
val bvshr : t -> t -> t


(** {Comparison Operators} *)

(** Equality *)
val equal : t -> t -> bool

(** Less than *)
val lt : t -> t -> bool

(** Greater than *)
val gt : t -> t -> bool

(** Less than or equal to *)
val lte : t -> t -> bool

(** Greater than or equal to *)
val gte : t -> t -> bool


(** {Pretty Printing} *)
(** Pretty-print a constant bitvector in SMTLIB decimal format *)
val pp_smtlib_print_bitvector : Format.formatter -> t -> unit

val pp_print_bitvector : Format.formatter -> t -> unit


(** {Conversions} *)
(** Convert a string to a bitvector
    Binary and hexadecimal notation is accepted as #b[01]+ and
    #x[0-9a-fA-F]+ as in the SMTLIB standard *)
val bitvector_of_string : string -> t

(** Convert a hashconsed string to a bitvector, store all converted
    values in a cache *)
val bitvector_of_hstring : HString.t -> t

(** Convert bitvector to string *)
val string_of_bitvector : t -> string


(** {Other functions} *)
(** Return the length of a bitvector *)
val length_of_bitvector : t -> int


(** {Discriminators} *)
(** Returns true for an unsigned machine integer of size 8 *)
val is_uint8 : t -> bool

(** Returns true for an unsigned machine integer of size 16 *)
val is_uint16 : t -> bool

(** Returns true for an unsigned machine integer of size 32 *)
val is_uint32 : t -> bool

(** Returns true for an unsigned machine integer of size 64 *)
val is_uint64 : t -> bool

(** Returns true for a signed machine integer of size 8 *)
val is_int8 : t -> bool
  
(** Returns true for a signed machine integer of size 16 *)
val is_int16 : t -> bool
  
(** Returns true for a signed machine integer of size 32 *)
val is_int32 : t -> bool
  
(** Returns true for a signed machine integer of size 64 *)
val is_int64 : t -> bool

(** Return true for an unsigned machine integer *)
val is_unsigned : t -> bool 

(** Return true for an unsigned machine integer *)
val is_signed : t -> bool


(** {Infix Operators} *)
(** Addition *)
val ( + ) : t -> t -> t

(** Subtraction *)
val ( - ) : t -> t -> t

(** Multiplication *)
val ( * ) : t -> t -> t

(** Division *)
val ( / ) : t -> t -> t

(** Remainder *)
val ( % ) : t -> t -> t

(** Shift left *)
val ( << ) : t -> t -> t

(** Shift right *)
val ( >> ) : t -> t -> t

(** Equality *)
val ( = ) : t -> t -> bool

(** Less than *)
val ( < ) : t -> t -> bool

(** Greater than *)
val ( > ) : t -> t -> bool

(** Less than or equal to *)
val ( <= ) : t -> t -> bool

(** Greater than or equal to *)
val ( >= ) : t -> t -> bool