open Format
open Lib

(* Bitvector type *)
type t =  
  | MUint8 of Stdint.Uint8.t
  | MUint16 of Stdint.Uint16.t
  | MUint32 of Stdint.Uint32.t
  | MUint64 of Stdint.Uint64.t
  | MInt8 of Stdint.Int8.t
  | MInt16 of Stdint.Int16.t
  | MInt32 of Stdint.Int32.t
  | MInt64 of Stdint.Int64.t

exception NonBinaryDigit
exception UnequalBVs
exception NonStandardBVSize


(* ********************************************************************** *)
(* Numeral -> Unsigned BV                                                 *)
(* ********************************************************************** *)

(* The mod operator in OCaml implements remainder 
with respect to numeral division. Since numeral division
in OCaml rounds toward 0, we design modulo which considers 
division that rounds toward negative infinity. 
For example, -1 mod 8 is -1 (with quotient 0) in OCaml, 
we want it to be 7 (with quotient -1).
While considering a mod b, the OCaml mod operator will do what 
we want when a and b are positive. The following function will 
additionally do what we want when a is negative; it wont do what 
we want when b is negative, but that's okay since 
we don't consider cases of 
modulo-n arithmetic where n is negative. *)
let modulo (x : Numeral.t) (y : Numeral.t) : Numeral.t =
  let result = (Numeral.rem x y) in
    if (Numeral.geq result Numeral.zero) then result
  else (Numeral.add result y)

(* Function that calculates the nth power of two *)
let rec pow2 (n : Numeral.t) : Numeral.t =
  if (Numeral.equal n Numeral.zero) then
    Numeral.one
  else
    Numeral.mult (Numeral.succ (Numeral.one)) 
                 (pow2 (Numeral.sub n Numeral.one))

(* Function that returns unsigned fixed-width int or bitvector version of a numeral *)
let num_to_ubv (size : Numeral.t) (i : Numeral.t) : t =
  (* m = 2^size for ubv(size), where we need to 
  do i modulo m on the input i *)
  let m = pow2 size in
  let n = modulo i m in
  let str_n = Numeral.string_of_numeral n in
    match (Numeral.to_int size) with 
    | 8 -> MUint8 (Stdint.Uint8.of_string str_n)
    | 16 -> MUint16 (Stdint.Uint16.of_string str_n)
    | 32 -> MUint32 (Stdint.Uint32.of_string str_n)
    | 64 -> MUint64 (Stdint.Uint64.of_string str_n)
    | _ -> raise NonStandardBVSize

  let num_to_ubv8 = num_to_ubv (Numeral.of_int 8)

  let num_to_ubv16 = num_to_ubv (Numeral.of_int 16)

  let num_to_ubv32 = num_to_ubv (Numeral.of_int 32)

  let num_to_ubv64 = num_to_ubv (Numeral.of_int 64)


(* ********************************************************************** *)
(* Numeral -> Signed BV                                                   *)
(* ********************************************************************** *)

(* Input any numeral n, input the size of the BV range, output the 
numeral fit into the range.For example, for 4-bit signed integers, 
input -9, 16 (2^4), and output 7 *)
let signed_modulo (n : Numeral.t) (range_size : Numeral.t) : Numeral.t = 
  let neg_lim = Numeral.neg (Numeral.div range_size (Numeral.of_int 2)) in
  let pos_lim = Numeral.sub (Numeral.div range_size (Numeral.of_int 2)) Numeral.one in 
    if (Numeral.lt n neg_lim) then
      let diff = (Numeral.sub neg_lim n) in
      let diff_mod = (Numeral.rem diff range_size) in
        if (Numeral.equal diff_mod Numeral.zero) then 
          neg_lim 
        else 
          (Numeral.sub pos_lim (Numeral.sub diff_mod Numeral.one))
    else if (Numeral.gt n pos_lim) then
      let diff = Numeral.sub n pos_lim in
      let diff_mod = (Numeral.rem diff range_size) in
        if(Numeral.equal diff_mod Numeral.zero) then 
          pos_lim
        else 
          Numeral.add neg_lim (Numeral.sub diff_mod Numeral.one)
    else n

(* Function that returns signed fixed-width int or bitvector version of a numeral *)
let num_to_bv (size : Numeral.t) (i : Numeral.t) : t =
  (* m =2^size for ubv(size), where we need to do 
     i modulo m on the input i *)
  let m = pow2 size in
  let n = signed_modulo i m in
  let str_n = Numeral.string_of_numeral n in
    match (Numeral.to_int size) with 
    | 8 -> MInt8 (Stdint.Int8.of_string str_n)
    | 16 -> MInt16 (Stdint.Int16.of_string str_n)
    | 32 -> MInt32 (Stdint.Int32.of_string str_n)
    | 64 -> MInt64 (Stdint.Int64.of_string str_n)
    | _ -> raise NonStandardBVSize

let num_to_bv8 = num_to_bv (Numeral.of_int 8) 

let num_to_bv16 = num_to_bv (Numeral.of_int 16)

let num_to_bv32 = num_to_bv (Numeral.of_int 32)

let num_to_bv64 = num_to_bv (Numeral.of_int 64)


(* ********************************************************************** *)
(* BV -> Numeral                                                 *)
(* ********************************************************************** *)
  
(*Function that returns the numeral corresponding to a bitvector *)
let bv_to_num (b : t) : Numeral.t =
  match b with
  | MUint8 i -> Numeral.of_string (Stdint.Uint8.to_string i)
  | MUint16 i -> Numeral.of_string (Stdint.Uint16.to_string i)
  | MUint32 i -> Numeral.of_string (Stdint.Uint32.to_string i)
  | MUint64 i -> Numeral.of_string (Stdint.Uint64.to_string i)
  | MInt8 i -> Numeral.of_string (Stdint.Int8.to_string i)
  | MInt16 i -> Numeral.of_string (Stdint.Int16.to_string i)
  | MInt32 i -> Numeral.of_string (Stdint.Int32.to_string i)
  | MInt64 i -> Numeral.of_string (Stdint.Int64.to_string i)


(* ********************************************************************** *)
(* Arithmetic Operations                                                  *)
(* ********************************************************************** *)

(* Addition *)
let add (x : t) (y : t) : t =
  match x, y with
    | MUint8 i, MUint8 j -> MUint8 (Stdint.Uint8.add i j)
    | MUint16 i, MUint16 j -> MUint16 (Stdint.Uint16.add i j)
    | MUint32 i, MUint32 j -> MUint32 (Stdint.Uint32.add i j)
    | MUint64 i, MUint64 j -> MUint64 (Stdint.Uint64.add i j)
    | MInt8 i, MInt8 j -> MInt8 (Stdint.Int8.add i j)
    | MInt16 i, MInt16 j -> MInt16 (Stdint.Int16.add i j)
    | MInt32 i, MInt32 j -> MInt32 (Stdint.Int32.add i j)
    | MInt64 i, MInt64 j -> MInt64 (Stdint.Int64.add i j)
    | _ -> raise UnequalBVs

(* Subtraction *)
let sub (x : t) (y : t) : t =
  match x, y with
    | MUint8 i, MUint8 j -> MUint8 (Stdint.Uint8.sub i j)
    | MUint16 i, MUint16 j -> MUint16 (Stdint.Uint16.sub i j)
    | MUint32 i, MUint32 j -> MUint32 (Stdint.Uint32.sub i j)
    | MUint64 i, MUint64 j -> MUint64 (Stdint.Uint64.sub i j)
    | MInt8 i, MInt8 j -> MInt8 (Stdint.Int8.sub i j)
    | MInt16 i, MInt16 j -> MInt16 (Stdint.Int16.sub i j)
    | MInt32 i, MInt32 j -> MInt32 (Stdint.Int32.sub i j)
    | MInt64 i, MInt64 j -> MInt64 (Stdint.Int64.sub i j)
    | _ -> raise UnequalBVs

(* Multiplication *)
let mul (x : t) (y : t) : t =
  match x, y with
    | MUint8 i, MUint8 j -> MUint8 (Stdint.Uint8.mul i j)
    | MUint16 i, MUint16 j -> MUint16 (Stdint.Uint16.mul i j)
    | MUint32 i, MUint32 j -> MUint32 (Stdint.Uint32.mul i j)
    | MUint64 i, MUint64 j -> MUint64 (Stdint.Uint64.mul i j)
    | MInt8 i, MInt8 j -> MInt8 (Stdint.Int8.mul i j)
    | MInt16 i, MInt16 j -> MInt16 (Stdint.Int16.mul i j)
    | MInt32 i, MInt32 j -> MInt32 (Stdint.Int32.mul i j)
    | MInt64 i, MInt64 j -> MInt64 (Stdint.Int64.mul i j)
    | _ -> raise UnequalBVs

(* Division *)
(* Raises Division_by_zero exception if second argument is zero *)
let div (x : t) (y : t) : t =
  match x, y with
    | MUint8 i, MUint8 j -> MUint8 (Stdint.Uint8.div i j)
    | MUint16 i, MUint16 j -> MUint16 (Stdint.Uint16.div i j)
    | MUint32 i, MUint32 j -> MUint32 (Stdint.Uint32.div i j)
    | MUint64 i, MUint64 j -> MUint64 (Stdint.Uint64.div i j)
    | MInt8 i, MInt8 j -> MInt8 (Stdint.Int8.div i j)
    | MInt16 i, MInt16 j -> MInt16 (Stdint.Int16.div i j)
    | MInt32 i, MInt32 j -> MInt32 (Stdint.Int32.div i j)
    | MInt64 i, MInt64 j -> MInt64 (Stdint.Int64.div i j)
    | _ -> raise UnequalBVs

(* Remainder *)
(* Raises Division_by_zero exception if second argument is zero *)
let rem (x : t) (y : t) : t =
  match x, y with
    | MUint8 i, MUint8 j -> MUint8 (Stdint.Uint8.rem i j)
    | MUint16 i, MUint16 j -> MUint16 (Stdint.Uint16.rem i j)
    | MUint32 i, MUint32 j -> MUint32 (Stdint.Uint32.rem i j)
    | MUint64 i, MUint64 j -> MUint64 (Stdint.Uint64.rem i j)
    | MInt8 i, MInt8 j -> MInt8 (Stdint.Int8.rem i j)
    | MInt16 i, MInt16 j -> MInt16 (Stdint.Int16.rem i j)
    | MInt32 i, MInt32 j -> MInt32 (Stdint.Int32.rem i j)
    | MInt64 i, MInt64 j -> MInt64 (Stdint.Int64.rem i j)
    | _ -> raise UnequalBVs

(* Negation *)
let neg (x : t) : t =
  match x with
    | MUint8 i -> MUint8 (Stdint.Uint8.neg i)
    | MUint16 i -> MUint16 (Stdint.Uint16.neg i)
    | MUint32 i -> MUint32 (Stdint.Uint32.neg i)
    | MUint64 i -> MUint64 (Stdint.Uint64.neg i)
    | MInt8 i -> MInt8 (Stdint.Int8.neg i)
    | MInt16 i -> MInt16 (Stdint.Int16.neg i)
    | MInt32 i -> MInt32 (Stdint.Int32.neg i)
    | MInt64 i -> MInt64 (Stdint.Int64.neg i)


(* ********************************************************************** *)
(* Logical Operations                                                     *)
(* ********************************************************************** *)

(* Bitwise and *)
let logand (x : t) (y : t) : t =
  match x, y with
    | MUint8 i, MUint8 j -> MUint8 (Stdint.Uint8.logand i j)
    | MUint16 i, MUint16 j -> MUint16 (Stdint.Uint16.logand i j)
    | MUint32 i, MUint32 j -> MUint32 (Stdint.Uint32.logand i j)
    | MUint64 i, MUint64 j -> MUint64 (Stdint.Uint64.logand i j)
    | MInt8 i, MInt8 j -> MInt8 (Stdint.Int8.logand i j)
    | MInt16 i, MInt16 j -> MInt16 (Stdint.Int16.logand i j)
    | MInt32 i, MInt32 j -> MInt32 (Stdint.Int32.logand i j)
    | MInt64 i, MInt64 j -> MInt64 (Stdint.Int64.logand i j)
    | _ -> raise UnequalBVs

(* Bitwise or *)
let logor (x : t) (y : t) : t =
  match x, y with
    | MUint8 i, MUint8 j -> MUint8 (Stdint.Uint8.logor i j)
    | MUint16 i, MUint16 j -> MUint16 (Stdint.Uint16.logor i j)
    | MUint32 i, MUint32 j -> MUint32 (Stdint.Uint32.logor i j)
    | MUint64 i, MUint64 j -> MUint64 (Stdint.Uint64.logor i j)
    | MInt8 i, MInt8 j -> MInt8 (Stdint.Int8.logor i j)
    | MInt16 i, MInt16 j -> MInt16 (Stdint.Int16.logor i j)
    | MInt32 i, MInt32 j -> MInt32 (Stdint.Int32.logor i j)
    | MInt64 i, MInt64 j -> MInt64 (Stdint.Int64.logor i j)
    | _ -> raise UnequalBVs

(* Bitwise not *)
let lognot (x : t) : t =
  match x with
    | MUint8 i -> MUint8 (Stdint.Uint8.lognot i)
    | MUint16 i -> MUint16 (Stdint.Uint16.lognot i)
    | MUint32 i -> MUint32 (Stdint.Uint32.lognot i)
    | MUint64 i -> MUint64 (Stdint.Uint64.lognot i)
    | MInt8 i -> MInt8 (Stdint.Int8.lognot i)
    | MInt16 i -> MInt16 (Stdint.Int16.lognot i)
    | MInt32 i -> MInt32 (Stdint.Int32.lognot i)
    | MInt64 i -> MInt64 (Stdint.Int64.lognot i)


(* ********************************************************************** *)
(* Conversion Operators                                                   *)
(* ********************************************************************** *)

(* uintN -> uint8 *)
let to_uint8 (x : t) : t = 
  match x with
  | MUint8 i -> MUint8 i
  | MUint16 i -> MUint8 (Stdint.Uint8.of_uint16 i)
  | MUint32 i -> MUint8 (Stdint.Uint8.of_uint32 i)
  | MUint64 i -> MUint8 (Stdint.Uint8.of_uint64 i)
  | _ -> raise NonStandardBVSize

(* uintN -> uint16 *)
let to_uint16 (x : t) : t = 
  match x with
  | MUint8 i -> MUint16 (Stdint.Uint16.of_uint8 i)
  | MUint16 i -> MUint16 i
  | MUint32 i -> MUint16 (Stdint.Uint16.of_uint32 i)
  | MUint64 i -> MUint16 (Stdint.Uint16.of_uint64 i)
  | _ -> raise NonStandardBVSize

(* uintN -> uint32 *)
let to_uint32 (x : t) : t = 
  match x with
  | MUint8 i -> MUint32 (Stdint.Uint32.of_uint8 i)
  | MUint16 i -> MUint32 (Stdint.Uint32.of_uint16 i)
  | MUint32 i -> MUint32 i
  | MUint64 i -> MUint32 (Stdint.Uint32.of_uint64 i)
  | _ -> raise NonStandardBVSize

(* uintN -> uint64 *)
let to_uint64 (x : t) : t = 
  match x with
  | MUint8 i -> MUint64 (Stdint.Uint64.of_uint8 i)
  | MUint16 i -> MUint64 (Stdint.Uint64.of_uint16 i)
  | MUint32 i -> MUint64 (Stdint.Uint64.of_uint32 i)
  | MUint64 i -> MUint64 i
  | _ -> raise NonStandardBVSize

(* intN -> int8 *)
let to_int8 (x : t) : t =
  match x with
  | MInt8 i -> MInt8 i
  | MInt16 i -> MInt8 (Stdint.Int8.of_int16 i)
  | MInt32 i -> MInt8 (Stdint.Int8.of_int32 i)
  | MInt64 i -> MInt8 (Stdint.Int8.of_int64 i)
  | _ -> raise NonStandardBVSize

(* intN -> int16 *)
let to_int16 (x : t) : t =
  match x with
  | MInt8 i -> MInt16 (Stdint.Int16.of_int8 i)
  | MInt16 i -> MInt16 i
  | MInt32 i -> MInt16 (Stdint.Int16.of_int32 i)
  | MInt64 i -> MInt16 (Stdint.Int16.of_int64 i)
  | _ -> raise NonStandardBVSize

(* intN -> int32 *)
let to_int32 (x : t) : t =
  match x with
  | MInt8 i -> MInt32 (Stdint.Int32.of_int8 i)
  | MInt16 i -> MInt32 (Stdint.Int32.of_int16 i)
  | MInt32 i -> MInt32 i
  | MInt64 i -> MInt32 (Stdint.Int32.of_int64 i)
  | _ -> raise NonStandardBVSize

(* intN -> int64 *)
let to_int64 (x : t) : t =
  match x with
  | MInt8 i -> MInt64 (Stdint.Int64.of_int8 i)
  | MInt16 i -> MInt64 (Stdint.Int64.of_int16 i)
  | MInt32 i -> MInt64 (Stdint.Int64.of_int32 i)
  | MInt64 i -> MInt64 i
  | _ -> raise NonStandardBVSize

(*  
(* Binary bitvector -> machine integer *)

(* Function that converts a binary character to numeral *)
let char_to_num (c : char) : Numeral.t =
  match c with 
  | '0' -> Numeral.zero
  | '1' -> Numeral.one
  | _ -> raise NonBinaryDigit

(* Function that calculates the nth power of two *)
let rec pow2 (n : Numeral.t) : Numeral.t =
  if (Numeral.equal n Numeral.zero) then
    Numeral.one
  else
    Numeral.mult (Numeral.succ (Numeral.one)) 
                 (pow2 (Numeral.sub n Numeral.one))

(*Function that returns the numeral corresponding to a binary bitvector *)
let rec num_of_bin_string_aux (s : string) (size : int) (acc : Numeral.t) : Numeral.t =
  if(size = 0) then acc else
    let new_acc = Numeral.add (Numeral.mult (pow2 (Numeral.of_int (size - 1))) (char_to_num s.[0])) acc in
      num_of_bin_string_aux (String.sub s 1 (size - 1) ) (size - 1) new_acc

let num_of_bin_string (s : string) (size : int) : Numeral.t =
  num_of_bin_string_aux s size Numeral.zero

let rec of_bin_string_aux (index : int) (size : int) (s : string) (acc : t) : t =
  if (s.[0] = "1" then 
let of_bin_string (s : string) (size : int) : t =
  match size with
  | 8 -> MUint8 Stdint.Uint8.of_string (Numeral.string_of_numeral (num_of_bin_string s 8))
  | 16 -> MUint16 (of_bin_string_aux 0 16 s (MUint16 0))
  | 32 -> MUint32 (of_bin_string_aux 0 32 s (MUint32 0))
  | 64 -> MUint64 (of_bin_string_aux 0 64 s (MUint64 0))
  | _ -> raise NonStandardBVSize
*)


(* ********************************************************************** *)
(* Shift operators                                                        *)
(* ********************************************************************** *)

(* Shift left *)
let bvshl (x : t) (y : t) : t =
  match x, y with
  | MUint8 i, MUint8 j ->
      if ((compare j (Stdint.Uint8.of_int 8)) >= 0) then
        MUint8 (Stdint.Uint8.zero)
      else
        MUint8 (Stdint.Uint8.shift_left i (Stdint.Uint8.to_int j))

  | MUint16 i, MUint16 j ->
      if ((compare j (Stdint.Uint16.of_int 16)) >= 0) then
        MUint16 (Stdint.Uint16.zero)
      else
        MUint16 (Stdint.Uint16.shift_left i (Stdint.Uint16.to_int j))

  | MUint32 i, MUint32 j ->
      if ((compare j (Stdint.Uint32.of_int 32)) >= 0) then
        MUint32 (Stdint.Uint32.zero)
      else
        MUint32 (Stdint.Uint32.shift_left i (Stdint.Uint32.to_int j))

  | MUint64 i, MUint64 j ->
      if ((compare j (Stdint.Uint64.of_int 64)) >= 0) then
        MUint64 (Stdint.Uint64.zero)
      else
        MUint64 (Stdint.Uint64.shift_left i (Stdint.Uint64.to_int j))

  | MInt8 i, MUint8 j ->
      if ((compare j (Stdint.Uint8.of_int 8)) >= 0) then
        MInt8 (Stdint.Int8.zero)
      else
        MInt8 (Stdint.Int8.shift_left i (Stdint.Uint8.to_int j))

  | MInt16 i, MUint16 j ->
      if ((compare j (Stdint.Uint16.of_int 16)) >= 0) then
        MInt16 (Stdint.Int16.zero)
      else
        MInt16 (Stdint.Int16.shift_left i (Stdint.Uint16.to_int j))

  | MInt32 i, MUint32 j ->
      if ((compare j (Stdint.Uint32.of_int 32)) >= 0) then
        MInt32 (Stdint.Int32.zero)
      else
        MInt32 (Stdint.Int32.shift_left i (Stdint.Uint32.to_int j))

  | MInt64 i, MUint64 j ->
      if ((compare j (Stdint.Uint64.of_int 64)) >= 0) then
        MInt64 (Stdint.Int64.zero)
      else
        MInt64 (Stdint.Int64.shift_left i (Stdint.Uint64.to_int j))

  | _, _ -> raise UnequalBVs


(* Shift right *)
let bvshr (x : t) (y : t) : t =
  match x, y with
  | MUint8 i, MUint8 j ->
      if ((compare j (Stdint.Uint8.of_int 8)) >= 0) then
        MUint8 (Stdint.Uint8.zero)
      else
        MUint8 (Stdint.Uint8.shift_right_logical i (Stdint.Uint8.to_int j))

  | MUint16 i, MUint16 j ->
      if ((compare j (Stdint.Uint16.of_int 16)) >= 0) then
        MUint16 (Stdint.Uint16.zero)
      else
        MUint16 (Stdint.Uint16.shift_right_logical i (Stdint.Uint16.to_int j))

  | MUint32 i, MUint32 j ->
      if ((compare j (Stdint.Uint32.of_int 32)) >= 0) then
        MUint32 (Stdint.Uint32.zero)
      else
        MUint32 (Stdint.Uint32.shift_right_logical i (Stdint.Uint32.to_int j))

  | MUint64 i, MUint64 j ->
      if ((compare j (Stdint.Uint64.of_int 64)) >= 0) then
        MUint64 (Stdint.Uint64.zero)
      else
        MUint64 (Stdint.Uint64.shift_right_logical i (Stdint.Uint64.to_int j))

  | MInt8 i, MUint8 j ->
      if ((compare j (Stdint.Uint8.of_int 8)) >= 0) then
        if((compare i (Stdint.Int8.zero)) < 0) then
          MInt8 (Stdint.Int8.of_int (-1))
        else
          MInt8 (Stdint.Int8.zero)
      else
        MInt8 (Stdint.Int8.shift_right i (Stdint.Uint8.to_int j))

  | MInt16 i, MUint16 j ->
      if ((compare j (Stdint.Uint16.of_int 16)) >= 0) then
        if((compare i (Stdint.Int16.zero)) < 0) then
          MInt16 (Stdint.Int16.of_int (-1))
        else
          MInt16 (Stdint.Int16.zero)
      else
        MInt16 (Stdint.Int16.shift_right i (Stdint.Uint16.to_int j))

  | MInt32 i, MUint32 j ->
      if ((compare j (Stdint.Uint32.of_int 32)) >= 0) then
        if((compare i (Stdint.Int32.zero)) < 0) then
          MInt32 (Stdint.Int32.of_int (-1))
        else
          MInt32 (Stdint.Int32.zero)
      else
        MInt32 (Stdint.Int32.shift_right i (Stdint.Uint32.to_int j))

  | MInt64 i, MUint64 j ->
      if ((compare j (Stdint.Uint64.of_int 64)) >= 0) then
        if((compare i (Stdint.Int64.zero)) < 0) then
          MInt64 (Stdint.Int64.of_int (-1))
        else
          MInt64 (Stdint.Int64.zero)
      else
        MInt64 (Stdint.Int64.shift_right i (Stdint.Uint64.to_int j))
  
  | _, _ -> raise UnequalBVs


(* ********************************************************************** *)
(* Comparison operators                                                   *)
(* ********************************************************************** *)

(* Equality *)
let equal (x : t) (y : t) : bool = 
  match x, y with
  | MUint8 i, MUint8 j -> (Stdint.Uint8.compare i j = 0)
  | MUint16 i, MUint16 j -> (Stdint.Uint16.compare i j = 0)
  | MUint32 i, MUint32 j -> (Stdint.Uint32.compare i j = 0)
  | MUint64 i, MUint64 j -> (Stdint.Uint64.compare i j = 0)
  | MInt8 i, MInt8 j -> (Stdint.Int8.compare i j = 0)
  | MInt16 i, MInt16 j -> (Stdint.Int16.compare i j = 0)
  | MInt32 i, MInt32 j -> (Stdint.Int32.compare i j = 0)
  | MInt64 i, MInt64 j -> (Stdint.Int64.compare i j = 0)
  | _, _ -> raise UnequalBVs

(* Less than *)
let lt (x : t) (y : t) : bool = 
  match x, y with
  | MUint8 i, MUint8 j -> (Stdint.Uint8.compare i j < 0)
  | MUint16 i, MUint16 j -> (Stdint.Uint16.compare i j < 0)
  | MUint32 i, MUint32 j -> (Stdint.Uint32.compare i j < 0)
  | MUint64 i, MUint64 j -> (Stdint.Uint64.compare i j < 0)
  | MInt8 i, MInt8 j -> (Stdint.Int8.compare i j < 0)
  | MInt16 i, MInt16 j -> (Stdint.Int16.compare i j < 0)
  | MInt32 i, MInt32 j -> (Stdint.Int32.compare i j < 0)
  | MInt64 i, MInt64 j -> (Stdint.Int64.compare i j < 0)
  | _, _ -> raise UnequalBVs

(* Greater than *)
let gt (x : t) (y : t) : bool = 
  match x, y with
  | MUint8 i, MUint8 j -> (Stdint.Uint8.compare i j > 0)
  | MUint16 i, MUint16 j -> (Stdint.Uint16.compare i j > 0)
  | MUint32 i, MUint32 j -> (Stdint.Uint32.compare i j > 0)
  | MUint64 i, MUint64 j -> (Stdint.Uint64.compare i j > 0)
  | MInt8 i, MInt8 j -> (Stdint.Int8.compare i j > 0)
  | MInt16 i, MInt16 j -> (Stdint.Int16.compare i j > 0)
  | MInt32 i, MInt32 j -> (Stdint.Int32.compare i j > 0)
  | MInt64 i, MInt64 j -> (Stdint.Int64.compare i j > 0)
  | _, _ -> raise UnequalBVs

(* Less than or equal to *)
let lte (x : t) (y : t) : bool = 
  match x, y with
  | MUint8 i, MUint8 j -> (Stdint.Uint8.compare i j <= 0)
  | MUint16 i, MUint16 j -> (Stdint.Uint16.compare i j <= 0)
  | MUint32 i, MUint32 j -> (Stdint.Uint32.compare i j <= 0)
  | MUint64 i, MUint64 j -> (Stdint.Uint64.compare i j <= 0)
  | MInt8 i, MInt8 j -> (Stdint.Int8.compare i j <= 0)
  | MInt16 i, MInt16 j -> (Stdint.Int16.compare i j <= 0)
  | MInt32 i, MInt32 j -> (Stdint.Int32.compare i j <= 0)
  | MInt64 i, MInt64 j -> (Stdint.Int64.compare i j <= 0)
  | _, _ -> raise UnequalBVs

(* Greater than or equal to *)
let gte (x : t) (y : t) : bool = 
  match x, y with
  | MUint8 i, MUint8 j -> (Stdint.Uint8.compare i j >= 0)
  | MUint16 i, MUint16 j -> (Stdint.Uint16.compare i j >= 0)
  | MUint32 i, MUint32 j -> (Stdint.Uint32.compare i j >= 0)
  | MUint64 i, MUint64 j -> (Stdint.Uint64.compare i j >= 0)
  | MInt8 i, MInt8 j -> (Stdint.Int8.compare i j >= 0)
  | MInt16 i, MInt16 j -> (Stdint.Int16.compare i j >= 0)
  | MInt32 i, MInt32 j -> (Stdint.Int32.compare i j >= 0)
  | MInt64 i, MInt64 j -> (Stdint.Int64.compare i j >= 0)
  | _, _ -> raise UnequalBVs


(* ********************************************************************** *)
(* Pretty-printing                                                        *)
(* ********************************************************************** *)

(* Pretty-print a bitvector in SMTLib decimal format *)
let pp_smtlib_print_bitvector ppf b =
  match b with 
  | MUint8 i -> fprintf ppf "(_ bv%s 8)" (Stdint.Uint8.to_string i)
  | MUint16 i -> fprintf ppf "(_ bv%s 16)" (Stdint.Uint16.to_string i)
  | MUint32 i -> fprintf ppf "(_ bv%s 32)" (Stdint.Uint32.to_string i)
  | MUint64 i -> fprintf ppf "(_ bv%s 64)" (Stdint.Uint64.to_string i)
  | MInt8 i -> fprintf ppf "(_ bv%s 8)" (Stdint.Uint8.to_string (Stdint.Uint8.of_int8 i))
  | MInt16 i -> fprintf ppf "(_ bv%s 16)" (Stdint.Uint16.to_string (Stdint.Uint16.of_int16 i))
  | MInt32 i -> fprintf ppf "(_ bv%s 32)" (Stdint.Uint32.to_string (Stdint.Uint32.of_int32 i))
  | MInt64 i -> fprintf ppf "(_ bv%s 64)" (Stdint.Uint64.to_string (Stdint.Uint64.of_int64 i))

      
(* ********************************************************************** *)
(* Conversions                                                            *)
(* ********************************************************************** *)

(* Convert a sequence of binary digits 
   beginning with "0b" to a constant bitvector *)
let bitvector_of_string_b (s : string) (size : int) : t =
  match size with
  | 8 -> MUint8 (Stdint.Uint8.of_string s) 
  | 16 -> MUint16 (Stdint.Uint16.of_string s)
  | 32 -> MUint32 (Stdint.Uint32.of_string s)
  | 64 -> MUint64 (Stdint.Uint64.of_string s)
  | _ -> raise NonStandardBVSize

(* Convert an SMTLib style bitvector literal 
   of the form (_ bvN S) where N is the decimal numeral
   representing the bitvector and S is its size
   to a constant bitvector *)
let bitvector_of_string_d (s : string) : t  = 
  let s_lst = String.split_on_char ' ' s in
  let bv_str = List.nth s_lst 1 in
  let size_str = List.nth s_lst 2 in
  let bv_str_len = ((String.length bv_str) - 2) in
    match size_str with
    | "8)" -> MUint8 (Stdint.Uint8.of_string (String.sub bv_str 2 bv_str_len))
    | "16)" -> MUint16 (Stdint.Uint16.of_string (String.sub bv_str 2 bv_str_len))
    | "32)" -> MUint32 (Stdint.Uint32.of_string (String.sub bv_str 2 bv_str_len))
    | "64)" -> MUint64 (Stdint.Uint64.of_string (String.sub bv_str 2 bv_str_len))
    | _ -> raise NonStandardBVSize

(* Convert a sequence of hex digits 
   beginning with "0x" to a constant bitvector *)
let bitvector_of_string_x (s : string) (size : int) : t =
  match size with
  | 8 -> MUint8 (Stdint.Uint8.of_string s)
  | 16 -> MUint16 (Stdint.Uint16.of_string s)
  | 32 -> MUint32 (Stdint.Uint32.of_string s)
  | 64 -> MUint64 (Stdint.Uint64.of_string s)
  | _ -> raise NonStandardBVSize

(* Convert a string to a constant bitvector *)
let bitvector_of_string s = 
  match 
    
    (* First two characters must be #b or #x *)
    (try 
       String.sub s 0 2 
     with
         Invalid_argument _ -> 
           raise (Invalid_argument "bitvector_of_string"))
      
  with 
      
    (* Convert from a binary string *)
    | "#b" -> let len = ((String.length s) - 2) in
              let str = (String.sub s 2 len) in
              let str_list = ["0b";str] in
              let new_str = String.concat "" str_list in
              (bitvector_of_string_b new_str len)
    
    | "0b" -> let len = ((String.length s) - 2) in
              (bitvector_of_string_b s len)

    (* Convert from a hexadecimal string *)
    | "#x" -> let len = ((String.length s) - 2) in
              let str = (String.sub s 2 len) in
              let str_list = ["0x";str] in
              let new_str = String.concat "" str_list in
              (bitvector_of_string_x new_str len)

    (* Convert from a decimal string *)
    | "(_" -> (bitvector_of_string_d s)
      
    (* Invalid prefix *)
    | _ -> raise (Invalid_argument "bitvector_of_string")

(* Cache for conversions of strings to bitvectors *)
let hstring_bitvector_cache = HString.HStringHashtbl.create 7

(* Convert a hashconsed string to a bitvector using the cache *)
let bitvector_of_hstring s =

  (* Return cached value if available *)
  try HString.HStringHashtbl.find hstring_bitvector_cache s with 

    | Not_found -> 
      
      (* Convert string to a bitvector *)
      let n = bitvector_of_string (HString.string_of_hstring s) in
      
      (* Add to cache *)
      HString.HStringHashtbl.add hstring_bitvector_cache s n;

      (* Return bitvector *)
      n


(* ********************************************************************** *)
(* Other functions                                                        *)
(* ********************************************************************** *)

(* Return length of bitvector *)
let length_of_bitvector (b : t) : int = 
  match b with
  | MUint8 _ | MInt8 _ -> 8
  | MUint16 _ | MInt16 _ -> 16
  | MUint32 _ | MInt32 _ -> 32
  | MUint64 _ | MInt64 _ -> 64


(* ********************************************************************** *)
(* Discriminators                                                         *)
(* ********************************************************************** *)

(* Return true if b is an unsigned machine integer of size 8 *)
let is_uint8 (b : t) : bool =
  match b with
  | MUint8 _ -> true
  | _ -> false

(* Return true if b is an unsigned machine integer of size 16 *)
let is_uint16 (b : t) : bool =
  match b with
  | MUint16 _ -> true
  | _ -> false

(* Return true if b is an unsigned machine integer of size 32 *)
let is_uint32 (b : t) : bool =
  match b with
  | MUint32 _ -> true
  | _ -> false

(* Return true if b is an unsigned machine integer of size 64 *)
let is_uint64 (b : t) : bool =
  match b with
  | MUint64 _ -> true
  | _ -> false

(* Return true if b is a signed machine integer of size 8 *)
let is_int8 (b : t) : bool =
  match b with
  | MInt8 _ -> true
  | _ -> false

(* Return true if b is a signed machine integer of size 16 *)
let is_int16 (b : t) : bool =
  match b with
  | MInt16 _ -> true
  | _ -> false

(* Return true if b is a signed machine integer of size 32 *)
let is_int32 (b : t) : bool =
  match b with
  | MInt32 _ -> true
  | _ -> false

(* Return true if b is a signed machine integer of size 64 *)
let is_int64 (b : t) : bool =
  match b with
  | MInt64 _ -> true
  | _ -> false

(* Return true if b is unsigned *)
let is_unsigned (b : t) : bool =
  match b with
  | MUint8 _ | MUint16 _ | MUint32 _ | MUint64 _ -> true
  | _ -> false

(* Return true if b is unsigned *)
let is_signed (b : t) : bool =
  match b with
  | MInt8 _ | MInt16 _ | MInt32 _ | MInt64 _ -> true
  | _ -> false


(* ********************************************************************** *)
(* Infix operators                                                        *)
(* ********************************************************************** *)

(* Addition *)
let ( + ) = add

(* Subtraction *)
let ( - ) = sub

(* Multiplication *)
let ( * ) = mul

(* Division *)
let ( / ) = div

(* Remainder *)
let ( % ) = rem

(* Shift left *)
let ( << ) = bvshl

(* Shift right *)
let ( >> ) = bvshr

(* Equality *)
let ( = ) = equal

(* Less than *)
let ( < ) = lt

(* Greater than *)
let ( > ) = gt

(* Less than or equal to *)
let ( <= ) = lte

(* Greater than or equal to *)
let ( >= ) = gte