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

(* Function that converts a single binary integer digit to Boolean *)
(*let bin_to_bool (digit : int) : bool =
  match digit with 
  | 0 -> false
  | 1 -> true
  | _ -> raise NonBinaryDigit*)

(* Function that inputs bit b, integer n, and repeats b n times *)
let rec repeat_bit (b : bool) (n : int) : t =
 match n with
 | 0 -> []
 | n -> b :: repeat_bit b (n - 1)

(* Function that returns the first bit of bitvector b *)
let first_bit (b : t) : bool =
  match b with
  | h :: t -> h
  | _ -> raise NonStandardBVSize

(* Function that extracts m down to n from the input bitvector *)
let rec bvextract (m : int) (n : int) (b : t) : t =
  let b_rev = (List.rev b) in
    if (m < n) then 
      raise NonStandardBVSize
    else if (n != 0) then
      raise NonStandardBVSize
    else
      match m with
      | 0 -> [List.hd b_rev]
      | m' -> (List.nth b_rev m') :: (bvextract (m' - 1) n b)

(* Function that sign extends the input bitvector by m bits *)
let rec bvsignext (m : int) (b : t) : t =
  let sign = List.hd b in
    let rec repeat (m : int) (b : bool) : t =
      match m with
      | 0 -> []
      | m' -> b :: repeat (m' - 1) b 
    in List.append (repeat m sign) b

(* Function that concatenates the input bitvectors *)
let rec bvconcat (b1 : t) (b2 : t) : t =
  List.append b1 b2


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
  let str_n = string_of_numeral n in
    match size with 
    | Numeral.of_int 8 -> Stdint.Uint8.of_string str_n
    | Numeral.of_int 16 -> Stdint.Uint16.of_string str_n
    | Numeral.of_int 32 -> Stdint.Uint32.of_string str_n
    | Numeral.of_int 64 -> Stdint.Uint64.of_string str_n
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
  let str_n = string_of_numeral n in
    match size with 
    | Numeral.of_int 8 -> Stdint.Int8.of_string str_n
    | Numeral.of_int 16 -> Stdint.Int16.of_string str_n
    | Numeral.of_int 32 -> Stdint.Int32.of_string str_n
    | Numeral.of_int 64 -> Stdint.Int64.of_string str_n
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
(* Constants                                                              *)
(* ********************************************************************** *)

(* The integer zero *)
let zero = 
  | MUint8 (Stdint.Uint8.zero)
  | MUint16 (Stdint.Uint16.zero)
  | MUint32 (Stdint.Uint32.zero)
  | MUint64 (Stdint.Uint64.zero)
  | MInt8 (Stdint.Int8.zero)
  | MInt16 (Stdint.Int16.zero)
  | MInt32 (Stdint.Int32.zero)
  | MInt64 (Stdint.Int64.zero)

(* The integer one *)
let one = 
  | MUint8 (Stdint.Uint8.one)
  | MUint16 (Stdint.Uint16.one)
  | MUint32 (Stdint.Uint32.one)
  | MUint64 (Stdint.Uint64.one)
  | MInt8 (Stdint.Int8.one)
  | MInt16 (Stdint.Int16.one)
  | MInt32 (Stdint.Int32.one)
  | MInt64 (Stdint.Int64.one)


(* ********************************************************************** *)
(* Arithmetic Operations                                                  *)
(* ********************************************************************** *)

(* Addition *)
let add (x : t) (y : t) : t =
  match x, y with
    | MUint8 i, MUint8 j -> MUint8 (Stdint.Uint8.add x y)
    | MUint16 i, MUint16 j -> MUint16 (Stdint.Uint16.add x y)
    | MUint32 i, MUint32 j -> MUint32 (Stdint.Uint32.add x y)
    | MUint64 i, MUint64 j -> MUint64 (Stdint.Uint64.add x y)
    | MInt8 i, MInt8 j -> MInt8 (Stdint.Int8.add x y)
    | MInt16 i, MInt16 j -> MInt16 (Stdint.Int16.add x y)
    | MInt32 i, MInt32 j -> MInt32 (Stdint.Int32.add x y)
    | MInt64 i, MInt64 j -> MInt64 (Stdint.Int64.add x y)
    | _ -> raise UnequalBVs

(* Subtraction *)
let sub (x : t) (y : t) : t =
  match x, y with
    | MUint8 i, MUint8 j -> MUint8 (Stdint.Uint8.sub x y)
    | MUint16 i, MUint16 j -> MUint16 (Stdint.Uint16.sub x y)
    | MUint32 i, MUint32 j -> MUint32 (Stdint.Uint32.sub x y)
    | MUint64 i, MUint64 j -> MUint64 (Stdint.Uint64.sub x y)
    | MInt8 i, MInt8 j -> MInt8 (Stdint.Int8.sub x y)
    | MInt16 i, MInt16 j -> MInt16 (Stdint.Int16.sub x y)
    | MInt32 i, MInt32 j -> MInt32 (Stdint.Int32.sub x y)
    | MInt64 i, MInt64 j -> MInt64 (Stdint.Int64.sub x y)
    | _ -> raise UnequalBVs

(* Multiplication *)
let mul (x : t) (y : t) : t =
  match x, y with
    | MUint8 i, MUint8 j -> MUint8 (Stdint.Uint8.mul x y)
    | MUint16 i, MUint16 j -> MUint16 (Stdint.Uint16.mul x y)
    | MUint32 i, MUint32 j -> MUint32 (Stdint.Uint32.mul x y)
    | MUint64 i, MUint64 j -> MUint64 (Stdint.Uint64.mul x y)
    | MInt8 i, MInt8 j -> MInt8 (Stdint.Int8.mul x y)
    | MInt16 i, MInt16 j -> MInt16 (Stdint.Int16.mul x y)
    | MInt32 i, MInt32 j -> MInt32 (Stdint.Int32.mul x y)
    | MInt64 i, MInt64 j -> MInt64 (Stdint.Int64.mul x y)
    | _ -> raise UnequalBVs

(* Division *)
(* Raises Division_by_zero exception if second argument is zero *)
let div (x : t) (y : t) : t =
  match x, y with
    | MUint8 i, MUint8 j -> MUint8 (Stdint.Uint8.div x y)
    | MUint16 i, MUint16 j -> MUint16 (Stdint.Uint16.div x y)
    | MUint32 i, MUint32 j -> MUint32 (Stdint.Uint32.div x y)
    | MUint64 i, MUint64 j -> MUint64 (Stdint.Uint64.div x y)
    | MInt8 i, MInt8 j -> MInt8 (Stdint.Int8.div x y)
    | MInt16 i, MInt16 j -> MInt16 (Stdint.Int16.div x y)
    | MInt32 i, MInt32 j -> MInt32 (Stdint.Int32.div x y)
    | MInt64 i, MInt64 j -> MInt64 (Stdint.Int64.div x y)
    | _ -> raise UnequalBVs

(* Remainder *)
(* Raises Division_by_zero exception if second argument is zero *)
let rem (x : t) (y : t) : t =
  match x, y with
    | MUint8 i, MUint8 j -> MUint8 (Stdint.Uint8.rem x y)
    | MUint16 i, MUint16 j -> MUint16 (Stdint.Uint16.rem x y)
    | MUint32 i, MUint32 j -> MUint32 (Stdint.Uint32.rem x y)
    | MUint64 i, MUint64 j -> MUint64 (Stdint.Uint64.rem x y)
    | MInt8 i, MInt8 j -> MInt8 (Stdint.Int8.rem x y)
    | MInt16 i, MInt16 j -> MInt16 (Stdint.Int16.rem x y)
    | MInt32 i, MInt32 j -> MInt32 (Stdint.Int32.rem x y)
    | MInt64 i, MInt64 j -> MInt64 (Stdint.Int64.rem x y)
    | _ -> raise UnequalBVs

(* Negation *)
let neg (x : t) : t =
  match x with
    | MUint8 i -> MUint8 (Stdint.Uint8.neg x)
    | MUint16 i -> MUint16 (Stdint.Uint16.neg x)
    | MUint32 i -> MUint32 (Stdint.Uint32.neg x)
    | MUint64 i -> MUint64 (Stdint.Uint64.neg x)
    | MInt8 i -> MInt8 (Stdint.Int8.neg x)
    | MInt16 i -> MInt16 (Stdint.Int16.neg x)
    | MInt32 i -> MInt32 (Stdint.Int32.neg x)
    | MInt64 i -> MInt64 (Stdint.Int64.neg x)


(* ********************************************************************** *)
(* Logical Operations                                                     *)
(* ********************************************************************** *)

(* Bitwise and *)
let logand (x : t) (y : t) : t =
  match x, y with
    | MUint8 i, MUint8 j -> MUint8 (Stdint.Uint8.logand x y)
    | MUint16 i, MUint16 j -> MUint16 (Stdint.Uint16.logand x y)
    | MUint32 i, MUint32 j -> MUint32 (Stdint.Uint32.logand x y)
    | MUint64 i, MUint64 j -> MUint64 (Stdint.Uint64.logand x y)
    | MInt8 i, MInt8 j -> MInt8 (Stdint.Int8.logand x y)
    | MInt16 i, MInt16 j -> MInt16 (Stdint.Int16.logand x y)
    | MInt32 i, MInt32 j -> MInt32 (Stdint.Int32.logand x y)
    | MInt64 i, MInt64 j -> MInt64 (Stdint.Int64.logand x y)
    | _ -> raise UnequalBVs

(* Bitwise or *)
let logor (x : t) (y : t) : t =
  match x, y with
    | MUint8 i, MUint8 j -> MUint8 (Stdint.Uint8.logor x y)
    | MUint16 i, MUint16 j -> MUint16 (Stdint.Uint16.logor x y)
    | MUint32 i, MUint32 j -> MUint32 (Stdint.Uint32.logor x y)
    | MUint64 i, MUint64 j -> MUint64 (Stdint.Uint64.logor x y)
    | MInt8 i, MInt8 j -> MInt8 (Stdint.Int8.logor x y)
    | MInt16 i, MInt16 j -> MInt16 (Stdint.Int16.logor x y)
    | MInt32 i, MInt32 j -> MInt32 (Stdint.Int32.logor x y)
    | MInt64 i, MInt64 j -> MInt64 (Stdint.Int64.logor x y)
    | _ -> raise UnequalBVs

(* Bitwise not *)
let lognot (x : t) : t =
  match x with
    | MUint8 i -> MUint8 (Stdint.Uint8.lognot x)
    | MUint16 i -> MUint16 (Stdint.Uint16.lognot x)
    | MUint32 i -> MUint32 (Stdint.Uint32.lognot x)
    | MUint64 i -> MUint64 (Stdint.Uint64.lognot x)
    | MInt8 i -> MInt8 (Stdint.Int8.lognot x)
    | MInt16 i -> MInt16 (Stdint.Int16.lognot x)
    | MInt32 i -> MInt32 (Stdint.Int32.lognot x)
    | MInt64 i -> MInt64 (Stdint.Int64.lognot x)


(* ********************************************************************** *)
(* Conversion Operators                                                   *)
(* ********************************************************************** *)

(* uintN -> uint8 *)
let to_uint8 (x : t) : t = 
  match x with
  | MUint8 i -> MUint8 i
  | MUint16 i -> MUint16 (Stdint.Uint8.of_uint16 i)
  | MUint32 i -> MUint32 (Stdint.Uint8.of_uint32 i)
  | MUint64 i -> MUint64 (Stdint.Uint8.of_uint64 i)
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
  | MInt16 i -> MInt8 (Stdint.Int8.of_uint16 i)
  | MInt32 i -> MInt8 (Stdint.Int8.of_uint32 i)
  | MInt64 i -> MInt8 (Stdint.Int8.of_uint64 i)
  | _ -> raise NonStandardBVSize

(* intN -> int16 *)
let to_int16 (x : t) : t =
  match x with
  | MInt8 i -> MInt16 (Stdint.Int16.of_uint8 i)
  | MInt16 i -> MInt16 i
  | MInt32 i -> MInt16 (Stdint.Int16.of_uint32 i)
  | MInt64 i -> MInt16 (Stdint.Int16.of_uint64 i)
  | _ -> raise NonStandardBVSize

(* intN -> int32 *)
let to_int32 (x : t) : t =
  match x with
  | MInt8 i -> MInt32 (Stdint.Int32.of_uint8 i)
  | MInt16 i -> MInt32 (Stdint.Int32.of_uint16 i)
  | MInt32 i -> MInt32 i
  | MInt64 i -> MInt32 (Stdint.Int32.of_uint64 i)
  | _ -> raise NonStandardBVSize

(* intN -> int64 *)
let to_int64 (x : t) : t =
  match x with
  | MInt8 i -> MInt64 (Stdint.Int64.of_uint8 i)
  | MInt16 i -> MInt64 (Stdint.Int64.of_uint16 i)
  | MInt32 i -> MInt64 (Stdint.Int64.of_uint32 i)
  | MInt64 i -> MInt64 i
  | _ -> raise NonStandardBVSize


(* ********************************************************************** *)
(* Shift operators                                                   *)
(* ********************************************************************** *)

(* Shift left *)
let bvshl (x : t) (y : t) : t =
  match x, y with
  | MUint8 i, MUint8 j ->
      if ((compare j (Stdint.Uint8.of_int 8)) >= 0) then
        Stdint.Uint8.zero
      else
        Stdint.Uint8.shift_left i (Stdint.Uint8.to_int j)

  | MUint16 i, MUint16 j ->
      if ((compare j (Stdint.Uint16.of_int 16)) >= 0) then
        Stdint.Uint16.zero
      else
        Stdint.Uint16.shift_left i (Stdint.Uint16.to_int j)

  | MUint32 i, MUint32 j ->
      if ((compare j (Stdint.Uint32.of_int 32)) >= 0) then
        Stdint.Uint32.zero
      else
        Stdint.Uint32.shift_left i (Stdint.Uint32.to_int j)

  | MUint64 i, MUint64 j ->
      if ((compare j (Stdint.Uint64.of_int 64)) >= 0) then
        Stdint.Uint64.zero
      else
        Stdint.Uint64.shift_left i (Stdint.Uint64.to_int j)

  | MInt8 i, MUint8 j ->
      if ((compare j (Stdint.Uint8.of_int 8)) >= 0) then
        Stdint.Int8.zero
      else
        Stdint.Int8.shift_left i (Stdint.Uint8.to_int j)

  | MInt16 i, MUint16 j ->
      if ((compare j (Stdint.Uint16.of_int 16)) >= 0) then
        Stdint.Int16.zero
      else
        Stdint.Int16.shift_left i (Stdint.Uint16.to_int j)

  | MInt32 i, MUint32 j ->
      if ((compare j (Stdint.Uint32.of_int 32)) >= 0) then
        Stdint.Int32.zero
      else
        Stdint.Int32.shift_left i (Stdint.Uint32.to_int j)

  | MInt64 i, MUint64 j ->
      if ((compare j (Stdint.Uint64.of_int 64)) >= 0) then
        Stdint.Int64.zero
      else
        Stdint.Int64.shift_left i (Stdint.Uint64.to_int j)

  | _, _ -> raise UnequalBVs


(* Shift right *)
let bvshl (x : t) (y : t) : t =
  match x, y with
  | MUint8 i, MUint8 j ->
      if ((compare j (Stdint.Uint8.of_int 8)) >= 0) then
        Stdint.Uint8.zero
      else
        Stdint.Uint8.shift_right_logical i (Stdint.Uint8.to_int j)

  | MUint16 i, MUint16 j ->
      if ((compare j (Stdint.Uint16.of_int 16)) >= 0) then
        Stdint.Uint16.zero
      else
        Stdint.Uint16.shift_right_logical i (Stdint.Uint16.to_int j)

  | MUint32 i, MUint32 j ->
      if ((compare j (Stdint.Uint32.of_int 32)) >= 0) then
        Stdint.Uint32.zero
      else
        Stdint.Uint32.shift_right_logical i (Stdint.Uint32.to_int j)

  | MUint64 i, MUint64 j ->
      if ((compare j (Stdint.Uint64.of_int 64)) >= 0) then
        Stdint.Uint64.zero
      else
        Stdint.Uint64.shift_right_logical i (Stdint.Uint64.to_int j)

  | MInt8 i, MUint8 j ->
      if ((compare j (Stdint.Uint8.of_int 8)) >= 0) then
        if((compare i (Stdint.Int8.zero)) < 0) then
          Stdint.Int8.of_int (-1)
        else
          Stdint.Int8.zero
      else
        Stdint.Int8.shift_right i (Stdint.Uint8.to_int j)

  | MInt16 i, MUint16 j ->
      if ((compare j (Stdint.Uint8.of_int 8)) >= 0) then
        if((compare i (Stdint.Int8.zero)) < 0) then
          Stdint.Int8.of_int (-1)
        else
          Stdint.Int8.zero
      else
        Stdint.Int8.shift_right i (Stdint.Uint8.to_int j)

  | MInt32 i, MUint32 j ->
      if ((compare j (Stdint.Uint32.of_int 32)) >= 0) then
        if((compare i (Stdint.Int32.zero)) < 0) then
          Stdint.Int32.of_int (-1)
        else
          Stdint.Int32.zero
      else
        Stdint.Int32.shift_right i (Stdint.Uint32.to_int j)

  | MInt64 i, MUint64 j ->
      if ((compare j (Stdint.Uint64.of_int 64)) >= 0) then
        if((compare i (Stdint.Int64.zero)) < 0) then
          Stdint.Int64.of_int (-1)
        else
          Stdint.Int64.zero
      else
        Stdint.Int64.shift_right i (Stdint.Uint64.to_int j)
  
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
(* Auxiliary Functions                                                    *)
(* ********************************************************************** *)

(* Return the first n elements of a list *)
let rec list_first_n' a l n =
  if n = 0 then a else 
    list_first_n' 
      ((try List.nth l (pred n) with 
        | Failure _ -> invalid_arg "list_first_n") :: a) 
      l
      (pred n) 

(* Return the first n elements of a list *)
let list_first_n l n = list_first_n' [] l n 


let rec list_from_n l n = 
  if n = 0 then l else
    list_from_n 
      (try List.tl l with Failure _ -> invalid_arg "list_from_n")
      (pred n)


(* ********************************************************************** *)
(* Pretty-printing                                                        *)
(* ********************************************************************** *)

(* Pretty-print a bitvector in binary format without #b prefix *)
let rec pp_print_bitvector_b' ppf = function 
  | [] -> ()
  | true :: tl -> pp_print_int ppf 1; pp_print_bitvector_b' ppf tl
  | false :: tl -> pp_print_int ppf 0; pp_print_bitvector_b' ppf tl

(* Pretty-print a bitvector in SMTLIB binary format *)
let pp_smtlib_print_bitvector_b ppf b = 
  fprintf ppf "#b%a" pp_print_bitvector_b' b


(* Pretty-print a bitvector in Yices' binary format *)
let pp_yices_print_bitvector_b ppf b = 
  fprintf ppf "0b%a" pp_print_bitvector_b' b

(* Pretty-print a bitvector in Yices' binary format given the decimal value and size *)
let pp_yices_print_bitvector_d ppf i s = 
  let size = (Numeral.to_int s) in
  let b = (match size with
    | 8 -> num_to_ubv8 i
    | 16 -> num_to_ubv16 i
    | 32 -> num_to_ubv32 i
    | 64 -> num_to_ubv64 i
    | _ -> raise NonStandardBVSize) 
  in
    fprintf ppf "0b%a" pp_print_bitvector_b' b

(* Pretty-print a bitvector in SMTLIB extended decimal format *)
let pp_smtlib_print_bitvector_d ppf n size = 
  fprintf ppf "(_ bv%s %s)" (Numeral.string_of_numeral n) (Numeral.string_of_numeral size)

(* Association list of bitvectors to hexadecimal digits *)
let bv_hex_table = 
  [([false; false; false; false], "0");
   ([false; false; false; true],  "1");
   ([false; false; true; false],  "2");
   ([false; false; true; true],   "3");
   ([false; true; false; false],  "4");
   ([false; true; false; true],   "5");
   ([false; true; true; false],   "6");
   ([false; true; true; true],    "7");
   ([true; false; false; false],  "8");
   ([true; false; false; true],   "9");
   ([true; false; true; false],   "A");
   ([true; false; true; true],    "B");
   ([true; true; false; false],   "C");
   ([true; true; false; true],    "D");
   ([true; true; true; false],    "E");
   ([true; true; true; true]),    "F"]

(* Pretty-print a bitvector in hexadecimal format *)
let rec pp_print_bitvector_x' ppf = function

  (* Print nothing for the empty bitvector *)
  | [] -> ()

  (* Pad with a false bit if less than four bits *)
  | d :: ([] as tl)
  | d :: ([_] as tl) 
  | d :: ([_; _] as tl) ->
    pp_print_bitvector_x' ppf (false :: d :: tl)

  (* Print a hexadecimal digit for the first four bits *)
  | d1 :: d2 :: d3 :: d4 :: tl -> 
    pp_print_string 
      ppf 
      (List.assoc ([d1; d2; d3; d4]) bv_hex_table);
    pp_print_bitvector_x' ppf tl

(* Pretty-print a bitvector in hexadecimal format *)
let pp_print_bitvector_x ppf b = 
  
  pp_print_string ppf "#X";
  
  match (List.length b) mod 4 with 
    | 0 -> pp_print_bitvector_x' ppf b
    | i -> 
      pp_print_bitvector_x' ppf (list_first_n b i);
      pp_print_bitvector_x' ppf (list_from_n b i)


(* Association list of hexadecimal digits to bitvectors *)
let hex_bv_table = 
  [("0", [false; false; false; false]);
   ("1", [false; false; false; true]);
   ("2", [false; false; true; false]);
   ("3", [false; false; true; true]);
   ("4", [false; true; false; false]);
   ("5", [false; true; false; true]);
   ("6", [false; true; true; false]);
   ("7", [false; true; true; true]);
   ("8", [true; false; false; false]);
   ("9", [true; false; false; true]);
   ("A", [true; false; true; false]);
   ("B", [true; false; true; true]);
   ("C", [true; true; false; false]);
   ("D", [true; true; false; true]);
   ("E", [true; true; true; false]);
   ("F", [true; true; true; true]);
   ("a", [true; false; true; false]);
   ("b", [true; false; true; true]);
   ("c", [true; true; false; false]);
   ("d", [true; true; false; true]);
   ("e", [true; true; true; false]);
   ("f", [true; true; true; true])] 

(* Convert a sequence of hexadecimal digits to a constant bitvector *)
let rec bitvector_of_string_x a i s = 
  
  if i <= 1 then a else
    
    try 

      bitvector_of_string_x
        ((List.assoc (String.sub s i 1) hex_bv_table ) @ a)
        (pred i)
        s

    with Not_found -> 

      raise (Invalid_argument "bitvector_of_string")
    
      
(* ********************************************************************** *)
(* Conversions                                                            *)
(* ********************************************************************** *)

(* Convert an OCaml integer to an infinite-precision integer numeral *)
let numeral_of_int i = HString.mk_hstring (Printf.sprintf "%i%!" i)

(* Constant zero *)
let num_zero = numeral_of_int 0

(* Constant one *)
let num_one = numeral_of_int 1

(* Convert an OCaml float to an infinite-precision real decimal *)
let decimal_of_float f = 

  if floor f = ceil f then 
    HString.mk_hstring (Printf.sprintf "%F0%!" f)
  else
    HString.mk_hstring (Printf.sprintf "%F%!" f)
      
(* Convert an infinite-precision integer numeral to an OCaml integer *)
let int_of_numeral n = int_of_string (HString.string_of_hstring n)

(* Convert an OCaml float to an infinite-precision real decimal *)
let float_of_decimal d = float_of_string (HString.string_of_hstring d)

(* Convert a bitvector to an integer *)
let int_of_bitvector b = 
  List.fold_left (fun a b -> a lsl 1 + (if b then 1 else 0)) 0 b

(* Convert a bitvector to an integer *)
let length_of_bitvector b = List.length b

(* A sequence of digits without leading zero *)
let numeral_of_string s = 

  try

    (* Scan string as a signed integer and discard*)
    Scanf.sscanf s "%_d%!" ();

    (* Return as string *)
    HString.mk_hstring s

  with 

    (* Raise exception if scanning fails *)
    | Scanf.Scan_failure _
    | End_of_file
    | Failure _ -> raise (Invalid_argument "smtlib_numeral_of_string")


(* A numeral followed by a decimal point followed by a sequence of digits *)
let decimal_of_string s = 
  
  try 
    
    (* Ensure that string consists of exactly a signed decimal, a
       decimal point and an unsigned decimal *)
    Scanf.sscanf s "%_d.%_u%!" ();

    (* Scan string as a floating point number and discard *)
    Scanf.sscanf s "%_f" ();

    (* Return as string *)
    HString.mk_hstring s

  with 

    (* Raise exception if scanning fails *)
    | Scanf.Scan_failure _
    | End_of_file
    | Failure _ -> raise (Invalid_argument "smtlib_decimal_of_string")


(* Convert a sequence of binary digits to a constant bitvector *)
let rec bitvector_of_string_b (a : t) (i : int) (s : string) : t = 

  if i <= 1 then a else
    
    match String.sub s i 1 with 

      | "0" -> bitvector_of_string_b (false :: a) (pred i) s
      | "1" -> bitvector_of_string_b (true :: a) (pred i) s
      | _ -> raise (Invalid_argument "bitvector_of_string")

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
    | "#b" | "0b" -> bitvector_of_string_b [] ((String.length s) - 1) s

    (* Convert from a hexadecimal string *)
    | "#x" -> bitvector_of_string_x [] ((String.length s) - 1) s

    (* Convert from a decimal string *)
    | "_ " -> 
      let f n s = (n, s) in
        let (n, s) =
          (try 
            Scanf.sscanf s "(_ bv%d %d)" f (*use %Ld and %u here to account for 64 bit ints*)
           with
           Scanf.Scan_failure _ -> 
            raise (Invalid_argument "bitvector_of_string"))
        in (num_to_bv (Numeral.of_int s) (Numeral.of_int n))
        (*with
          | "bv" -> [false;false;true;true]

            let len = String.length s in
              let lenminus1 = (len - 1) in

              match
              (try 
                String.sub s lenminus1 1)
               with
                Invalid_argument _ ->
                  raise (Invalid_argument "bitvector_of_string"))

              with 
                | ")" ->

                  let substr = String.sub s 5 (len - 6) in
                    let num_list = (String.split_on_char ' ' substr) in
                      let n = (List.nth num_list 0) in
                        let s = (List.nth num_list 1) in
                          let n_num = (int_of_string n) in
                            let s_num = (int_of_string s) in
                              int_to_bv s_num n_num
                
                | _ -> raise (Invalid_argument "bitvector_of_string")
          
          | _ -> raise (Invalid_argument "bitvector_of_string")*)

    (* Invalid prefix *)
    | _ -> raise (Invalid_argument "bitvector_of_string")


(* Cache for conversions of strings to numerals *)
let hstring_numeral_cache = HString.HStringHashtbl.create 7

(* Convert a hashconsed string to a numeral using the cache *)
let numeral_of_hstring s =

  (* Return cached value if available *)
  try HString.HStringHashtbl.find hstring_numeral_cache s with 

    | Not_found -> 
      
      (* Convert string to a numeral *)
      let n = numeral_of_string (HString.string_of_hstring s) in
      
      (* Add to cache *)
      HString.HStringHashtbl.add hstring_numeral_cache s n;

      (* Return numeral *)
      n

(* Cache for conversions of strings to decimals *)
let hstring_decimal_cache = HString.HStringHashtbl.create 7

(* Convert a hashconsed string to a decimal using the cache *)
let decimal_of_hstring s =

  (* Return cached value if available *)
  try HString.HStringHashtbl.find hstring_decimal_cache s with 

    | Not_found -> 
      
      (* Convert string to a decimal *)
      let n = decimal_of_string (HString.string_of_hstring s) in
      
      (* Add to cache *)
      HString.HStringHashtbl.add hstring_decimal_cache s n;

      (* Return decimal *)
      n

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

(* Convert an infinite-precision integer numeral to a string *)
let string_of_numeral s = HString.string_of_hstring s 

(* Convert an infinite-precision real decimal to a string *)
let string_of_decimal s = HString.string_of_hstring s 

(* Convert a hashconsed string to a Boolean value *)
let bool_of_hstring s = bool_of_string (HString.string_of_hstring s) 


(* ********************************************************************** *)
(* Infix comparison operators                                             *)
(* ********************************************************************** *)

(* Equality *)
let ( = ) = equal

(* Signed lesser than *)
let ( < ) = lt

(* Signed greater than *)
let ( > ) = gt

(* Signed lesser than or equal to *)
let ( <= ) = lte

(* Signed greater than or equal to *)
let ( >= ) = gte