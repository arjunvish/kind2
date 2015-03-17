(* This file is part of the Kind 2 model checker.

   Copyright (c) 2014 by the Board of Trustees of the University of Iowa

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

open Lib


(* ********************************************************************** *)
(* Dummy and default values                                               *)
(* ********************************************************************** *)

(* Command line options *)
let cmd_line () = [| |]

(* Dummy implementation *)
let check_sat_limited_cmd ms = failwith "Not implemented"

(* Dummy implementation *)
let check_sat_assuming_cmd ms = failwith "Not implemented"

(* Dummy implementation *)
let check_sat_assuming_supported () = failwith "Not implemented"

(* Headers to send after creating solver instance *)
let headers () = []

(* Extension for trace file *)
let trace_extension = "smt2"

(* Comment deliminters for trace file *)
let comment_delims = ";;", ""


(* ********************************************************************** *)
(* Non-SMTLIB specific functions                                          *)
(* ********************************************************************** *)

(* Conversions for gen_expr_of_string_sexpr

   Defaults constants and functions for vanilla SMTLIB format are
   below, override in specific driver.
*)
type expr_of_string_sexpr_conv =

  { (* String constant for let keyword *) 
    s_let : HString.t;

    (* String constant for forall keyword *) 
    s_forall : HString.t;

    (* String constant for exists keyword *) 
    s_exists : HString.t;

    (* String constant for division operator *) 
    s_div : HString.t;

    (* String constant for unary minus operator *) 
    s_minus : HString.t;

    (* String constant for define-fun keyword *) 
    s_define_fun : HString.t;

    (* Conversion of an S-expression atom to a term *)
    const_of_atom : (HString.t * Var.t) list -> HString.t -> Term.t;

    (* Conversion of a string to a symbol *)
    symbol_of_atom : HString.t -> Symbol.t;

    (* Conversion of an S-expression to a type *)
    type_of_sexpr : HStringSExpr.t -> Type.t;

    (* Conversion of an S-expression to an expression *)
    expr_of_string_sexpr : 
      expr_of_string_sexpr_conv -> 
      (HString.t * Var.t) list -> 
      HStringSExpr.t -> 
      Term.t;

    (* Conversion of an S-expression to a lambda abstraction *)
    expr_or_lambda_of_string_sexpr : 
      expr_of_string_sexpr_conv -> 
      (HString.t * Var.t) list -> 
      HStringSExpr.t -> 
      (HString.t * Model.term_or_lambda)
  }


(* Convert a list of bindings *)
let rec gen_bindings_of_string_sexpr
    ({ expr_of_string_sexpr } as conv) 
    b 
    accum = 

  function 

    (* All bindings consumed: return accumulator in original order *)
    | [] -> List.rev accum

    (* Take first binding *)
    | HStringSExpr.List [HStringSExpr.Atom var; expr] :: tl -> 

      (* Convert to an expression *)
      let expr = expr_of_string_sexpr conv b expr in

      (* Get the type of the expression *)
      let expr_type = Term.type_of_term expr in

      (* Create a variable of the identifier and the type of
         the expression *)
      let tvar = Var.mk_free_var var expr_type in

      (* Add bound expresssion to accumulator *)
      gen_bindings_of_string_sexpr conv b ((tvar, expr) :: accum) tl

    (* Expression must be a pair *)
    | e :: _ -> 

      failwith 
        ("Invalid expression in let binding: " ^
         (string_of_t HStringSExpr.pp_print_sexpr e))


(* Convert a list of typed variables *)
let rec gen_bound_vars_of_string_sexpr
    ({ type_of_sexpr } as conv)
    b
    accum =

  function 

    (* All bindings consumed: return accumulator in original order *)
    | [] -> List.rev accum

    (* Take first binding *)
    | HStringSExpr.List [HStringSExpr.Atom v; t] :: tl -> 

      (* Get the type of the expression *)
      let var_type = type_of_sexpr t in

      (* Create a variable of the identifier and the type of the expression *)
      let tvar = Var.mk_free_var v var_type in

      (* Add bound expresssion to accumulator *)
      gen_bound_vars_of_string_sexpr conv b (tvar :: accum) tl

    (* Expression must be a pair *)
    | e :: _ -> 

      failwith 
        ("Invalid expression in let binding: " ^
         (string_of_t HStringSExpr.pp_print_sexpr e))


(* Convert a string S-expression to an expression 

   This function is generic, and also used from {!YicesDriver} *)
let gen_expr_of_string_sexpr' 
    ({ s_let; 
       s_forall; 
       s_exists; 
       s_div; 
       s_minus; 
       const_of_atom; 
       symbol_of_atom;
       expr_of_string_sexpr;
       expr_or_lambda_of_string_sexpr } as conv)
    bound_vars = 

  function 

    (* An empty list *)
    | HStringSExpr.List [] -> 

      (* Cannot convert to an expression *)
      failwith "Invalid Nil in S-expression"


    (* An list with a list as first element *)
    | HStringSExpr.List (HStringSExpr.List _ :: _) -> 

      (* Cannot convert to an expression *)
      failwith "Invalid S-expression"


    (* A singleton list: treat as atom *)
    | HStringSExpr.List [e] -> 

      expr_of_string_sexpr conv bound_vars e

    (* A let binding *)
    | HStringSExpr.List 
        ((HStringSExpr.Atom s) :: [HStringSExpr.List v; t]) 
      when s == s_let -> 

      (* Convert bindings and obtain a list of bound variables *)
      let bindings = gen_bindings_of_string_sexpr conv bound_vars [] v in

      (* Convert bindings to an association list from strings to
         variables *)
      let bound_vars' = 
        List.map 
          (function (v, _) -> (Var.hstring_of_free_var v, v))
          bindings 
      in

      (* Parse the subterm, giving an association list of bound
         variables and return a let bound term *)
      Term.mk_let 
        bindings
        (expr_of_string_sexpr conv (bound_vars @ bound_vars') t)


    (* A universal or existential quantifier *)
    | HStringSExpr.List 
        ((HStringSExpr.Atom s) :: [HStringSExpr.List v; t]) 
      when s == s_forall || s == s_exists -> 

      (* Get list of variables bound by the quantifier *)
      let quantified_vars = 
        gen_bound_vars_of_string_sexpr conv bound_vars [] v 
      in

      (* Convert bindings to an association list from strings to
         variables *)
      let bound_vars' = 
        List.map 
          (function v -> (Var.hstring_of_free_var v, v))
          quantified_vars
      in

      (* Parse the subterm, giving an association list of bound variables
         and return a universally or existenially quantified term *)
      (if s == s_forall then Term.mk_forall 
       else if s == s_exists then Term.mk_exists
       else assert false)
        quantified_vars
        (expr_of_string_sexpr conv (bound_vars @ bound_vars') t)


    (* Parse (/ n d) as rational constant *)
    | HStringSExpr.List
        [HStringSExpr.Atom s; HStringSExpr.Atom n; HStringSExpr.Atom d] 
      when s == s_div && 
           (try
              let _ =
                Numeral.of_string (HString.string_of_hstring n) 
              in
              true
            with _ -> false) &&
           (try
              let _ =
                Numeral.of_string (HString.string_of_hstring d) 
              in
              true
            with _ -> false) ->

      Term.mk_dec
        Decimal.
          ((HString.string_of_hstring n |> of_string) /
           (HString.string_of_hstring d |> of_string))


    (* Parse (/ (- n) d) as rational constant *)
    | HStringSExpr.List
        [HStringSExpr.Atom s2;
         HStringSExpr.List [HStringSExpr.Atom s1; HStringSExpr.Atom n]; 
         HStringSExpr.Atom d] 
      when s1 == s_minus && 
           s2 == s_div && 
           (try
              let _ =
                Numeral.of_string (HString.string_of_hstring n) 
              in
              true
            with _ -> false) &&
           (try
              let _ =
                Numeral.of_string (HString.string_of_hstring d) 
              in
              true
            with _ -> false) ->

      Term.mk_dec
        Decimal.
          (- 
          (HString.string_of_hstring n |> of_string) /
          (HString.string_of_hstring d |> of_string))


    (* Atom or singleton list *)
    | HStringSExpr.Atom s ->

      (* Leaf in the symbol tree *)
      (const_of_atom bound_vars s)

    (*  A list with more than one element *)
    | HStringSExpr.List ((HStringSExpr.Atom h) :: tl) -> 

      (

        (* Symbol from string *)
        let s = 

          try 

            (* Map the string to an interpreted function symbol *)
            symbol_of_atom h 

          with 

            (* Function symbol is uninterpreted *)
            | Not_found -> 

              (* Uninterpreted symbol from string *)
              let u = 

                try 

                  UfSymbol.uf_symbol_of_string (HString.string_of_hstring h)

                with Not_found -> 

                  (* Cannot convert to an expression *)
                  failwith 
                    (Format.sprintf 
                       "Undeclared uninterpreted function symbol %s in \
                        S-expression"
                       (HString.string_of_hstring h))
              in

              (* Get the uninterpreted symbol of the string *)
              Symbol.mk_symbol (`UF u)


        in

        (* Create an application of the function symbol to the subterms *)
        let t = 
          Term.mk_app
            s
            (List.map (expr_of_string_sexpr conv bound_vars) tl)
        in

        (* Convert (= 0 (mod t n)) to (t divisible n) *)
        Term.mod_to_divisible t

      )

           

(* Convert a string S-expression to a lambda abstraction 

   This function is generic, and also used from {!YicesDriver} *)
let gen_expr_or_lambda_of_string_sexpr' ({ s_define_fun } as conv) bound_vars = 

  function 

    (* (define-fun c () Bool t) *)
    | HStringSExpr.List 
        [HStringSExpr.Atom s; (* define-fun *)
         HStringSExpr.Atom i; (* identifier *)
         HStringSExpr.List []; (* Parameters *)
         _; (* Result type *)
         t (* Expression *)
        ]
      when s == s_define_fun -> 

      (i, 
       Model.Term
         (gen_expr_of_string_sexpr' conv bound_vars t))


    (* (define-fun A ((x1 Int) (x2 Int)) Bool t) *)
    | HStringSExpr.List 
        [HStringSExpr.Atom s; (* define-fun *)
         HStringSExpr.Atom i; (* identifier *)
         HStringSExpr.List v; (* Parameters *)
         _; (* Result type *)
         t (* Expression *)
        ]
      when s == s_define_fun -> 

      (* Get list of variables bound by the quantifier *)
      let vars = gen_bound_vars_of_string_sexpr conv bound_vars [] v in

      (* Convert bindings to an association list from strings to
         variables *)
      let bound_vars' = 
        List.map 
          (function v -> (Var.hstring_of_free_var v, v))
          vars
      in

      (i,
       Model.Lambda
         (Term.mk_lambda
            vars
            (gen_expr_of_string_sexpr' conv (bound_vars @ bound_vars') t)))

    | _ -> invalid_arg "gen_expr_of_lambda_string_sexpr"


(* Call function with an empty list of bound variables *)      
let gen_expr_of_string_sexpr conv = 
  gen_expr_of_string_sexpr' conv [] 

(* Call function with an empty list of bound variables *)      
let gen_expr_or_lambda_of_string_sexpr conv = 
  gen_expr_or_lambda_of_string_sexpr' conv [] 


(* ********************************************************************** *)
(* SMTLIB specific conversions                                            *)
(* ********************************************************************** *)

(* Convert a logic to a string *)
let string_of_logic = TermLib.string_of_logic

(* Pretty-print a logic identifier *)
let pp_print_logic = TermLib.pp_print_logic

(* Convert type *)
let interpr_type t = match Type.node_of_type t with

  | Type.IntRange _ -> Type.mk_int ()

  | Type.Bool | Type.Int | Type.Real -> t

  | _ -> failwith ((Type.string_of_type t)^" not supported")


(* Pretty-print a sort *)
let pp_print_sort ppf t = Type.pp_print_type ppf (interpr_type t)

(* Return a string representation of a sort *)
let string_of_sort = string_of_t pp_print_sort


(* Association list of strings to function symbols *) 
let smtlib_string_symbol_list =
  [("not", Symbol.mk_symbol `NOT);
   ("=>", Symbol.mk_symbol `IMPLIES);
   ("and", Symbol.mk_symbol `AND);
   ("or", Symbol.mk_symbol `OR);
   ("xor", Symbol.mk_symbol `XOR);
   ("=", Symbol.mk_symbol `EQ);
   ("distinct", Symbol.mk_symbol `DISTINCT);
   ("ite", Symbol.mk_symbol `ITE);
   ("-", Symbol.mk_symbol `MINUS);
   ("+", Symbol.mk_symbol `PLUS);
   ("*", Symbol.mk_symbol `TIMES);
   ("/", Symbol.mk_symbol `DIV);
   ("div", Symbol.mk_symbol `INTDIV);
   ("mod", Symbol.mk_symbol `MOD);
   ("abs", Symbol.mk_symbol `ABS);
   ("<=", Symbol.mk_symbol `LEQ);
   ("<", Symbol.mk_symbol `LT);
   (">=", Symbol.mk_symbol `GEQ);
   (">", Symbol.mk_symbol `GT);
   ("to_real", Symbol.mk_symbol `TO_REAL);
   ("to_int", Symbol.mk_symbol `TO_INT);
   ("is_int", Symbol.mk_symbol `IS_INT);
(*
   ("concat", Symbol.mk_symbol `CONCAT);
   ("bvnot", Symbol.mk_symbol `BVNOT);
   ("bvneg", Symbol.mk_symbol `BVNEG);
   ("bvand", Symbol.mk_symbol `BVAND);
   ("bvor", Symbol.mk_symbol `BVOR);
   ("bvadd", Symbol.mk_symbol `BVADD);
   ("bvmul", Symbol.mk_symbol `BVMUL);
   ("bvdiv", Symbol.mk_symbol `BVDIV);
   ("bvurem", Symbol.mk_symbol `BVUREM);
   ("bvshl", Symbol.mk_symbol `BVSHL);
   ("bvlshr", Symbol.mk_symbol `BVLSHR);
   ("bvult", Symbol.mk_symbol `BVULT);
*)
   ("select", Symbol.mk_symbol `SELECT);
(*
   ("store", Symbol.mk_symbol `STORE)
*)

  ]

(* Reserved words that we don't support *)
let smtlib_reserved_word_list = 
  List.map 
    HString.mk_hstring 
    ["par"; "_"; "!"; "as" ]

(* Hashtable for hashconsed strings to function symbols *)
let hstring_symbol_table = HString.HStringHashtbl.create 50 


(* Populate hashtable with hashconsed strings and their symbol *)
let _ = 
  List.iter
    (function (s, v) -> 
      HString.HStringHashtbl.add 
        hstring_symbol_table 
        (HString.mk_hstring s)
        v)
    smtlib_string_symbol_list 


(* Pretty-print a symbol *)
let rec pp_print_symbol_node ?arity ppf = function 

  | `TRUE -> Format.pp_print_string ppf "true"
  | `FALSE -> Format.pp_print_string ppf "false"
  | `NOT -> Format.pp_print_string ppf "not"
  | `IMPLIES -> Format.pp_print_string ppf "=>"
  | `AND  -> Format.pp_print_string ppf "and"
  | `OR -> Format.pp_print_string ppf "or"
  | `XOR -> Format.pp_print_string ppf "xor"

  | `EQ -> Format.pp_print_string ppf "="
  | `DISTINCT -> Format.pp_print_string ppf "distinct"
  | `ITE -> Format.pp_print_string ppf "ite" 

  | `NUMERAL i -> Numeral.pp_print_numeral_sexpr ppf i
  | `DECIMAL f -> Decimal.pp_print_decimal_sexpr ppf f
(*
  | `BV b -> pp_smtlib_print_bitvector_b ppf b
*)
  | `MINUS -> Format.pp_print_string ppf "-"
  | `PLUS -> Format.pp_print_string ppf "+"
  | `TIMES -> Format.pp_print_string ppf "*"
  | `DIV -> Format.pp_print_string ppf "/"
  | `INTDIV -> Format.pp_print_string ppf "div"
  | `MOD -> Format.pp_print_string ppf "mod"
  | `ABS -> Format.pp_print_string ppf "abs"

  | `LEQ -> Format.pp_print_string ppf "<="
  | `LT -> Format.pp_print_string ppf "<"
  | `GEQ -> Format.pp_print_string ppf ">="
  | `GT -> Format.pp_print_string ppf ">"

  | `TO_REAL -> Format.pp_print_string ppf "to_real"
  | `TO_INT -> Format.pp_print_string ppf "to_int"
  | `IS_INT -> Format.pp_print_string ppf "is_int"

  | `DIVISIBLE n -> 
    Format.pp_print_string ppf "divisible";
    Format.pp_print_space ppf ();
    Numeral.pp_print_numeral ppf n
(*
  | `CONCAT -> Format.pp_print_string ppf "concat"
  | `EXTRACT (i, j) -> 
    Format.fprintf 
      ppf 
      "(_ extract %a %a)" 
      Numeral.pp_print_numeral i
      Numeral.pp_print_numeral j

  | `BVNOT -> Format.pp_print_string ppf "bvnot"
  | `BVNEG -> Format.pp_print_string ppf "bvneg"
  | `BVAND -> Format.pp_print_string ppf "bvand"
  | `BVOR -> Format.pp_print_string ppf "bvor"
  | `BVADD -> Format.pp_print_string ppf "bvadd"
  | `BVMUL -> Format.pp_print_string ppf "bvmul"
  | `BVDIV -> Format.pp_print_string ppf "bvdiv"
  | `BVUREM -> Format.pp_print_string ppf "bvurem"
  | `BVSHL -> Format.pp_print_string ppf "bvshl"
  | `BVLSHR -> Format.pp_print_string ppf "bvlshr"
  | `BVULT -> Format.pp_print_string ppf "bvult"
*)
  | `SELECT -> Format.pp_print_string ppf "select"
(*
  | `STORE -> Format.pp_print_string ppf "store"
*)
  | `UF u -> UfSymbol.pp_print_uf_symbol ppf u

(* Pretty-print a hashconsed symbol *)
and pp_print_symbol ?arity ppf s =
  pp_print_symbol_node ?arity ppf (Symbol.node_of_symbol s)


(* Return a string representation of a hashconsed symbol *)
let string_of_symbol ?arity s = string_of_t (pp_print_symbol ?arity) s


let pp_print_term ppf t =
  Term.T.pp_print_term_w pp_print_symbol ppf t
        
    
(* Pretty-print an expression *)
let pp_print_expr = pp_print_term


(* Pretty-print an expression to the standard formatter *)
let print_expr = pp_print_expr Format.std_formatter


(* Return a string representation of an expression *)
let string_of_expr t = string_of_t pp_print_expr t


(* Lookup symbol of a hashconsed string *)
let symbol_of_smtlib_atom s = 

  try 

    (* Map hashconsed string to symbol *)
    HString.HStringHashtbl.find hstring_symbol_table s

  (* String is not one of our symbols *)
  with Not_found -> 

    (* Check if string is a reserved word *)
    if List.memq s smtlib_reserved_word_list then 

      (* Cannot parse S-expression *)
      raise 
        (Invalid_argument 
           (Format.sprintf 
              "Unsupported reserved word '%s' in S-expression"
              (HString.string_of_hstring s)))

    else

      (* String is not a symbol *)
      raise Not_found 



(* Convert a string to a postive numeral or decimal

   The first argument is an association list of strings to variables
   that are currently bound to distinguish between uninterpreted
   function symbols and variables. *)

let const_of_smtlib_atom b t = 

  let res = 

    (* Empty strings are invalid *)
    if HString.length t = 0 then

      (* String is empty *)
      raise (Invalid_argument "num_expr_of_smtlib_token")

    else

      try

        (* Return numeral of string *)
        Term.mk_num (Numeral.of_string (HString.string_of_hstring t))

      (* String is not a decimal *)
      with Invalid_argument _ -> 

        try 

          (* Return decimal of string *)
          Term.mk_dec (Decimal.of_string (HString.string_of_hstring t))

        with Invalid_argument _ -> 

          try 

            (* Return decimal of string *)
            Term.mk_dec (Decimal.of_num (Num.num_of_string
                                           (HString.string_of_hstring t)))

          with Invalid_argument _ | Failure "num_of_string" -> 
(*
            try 

              (* Return bitvector of string *)
              Term.mk_bv (bitvector_of_hstring t)

            with Invalid_argument _ -> 
*)
              try 

                (* Return symbol of string *)
                Term.mk_bool (bool_of_hstring t)

              (* String is not an interpreted symbol *)
              with Invalid_argument _ -> 

                try 

                  (* Return bound symbol *)
                  Term.mk_var (List.assq t b)

                (* String is not a bound variable *)
                with Not_found -> 

                  try 

                    (* Return uninterpreted constant *)
                    Term.mk_uf 
                      (UfSymbol.uf_symbol_of_string
                         (HString.string_of_hstring t))
                      []

                  with Not_found -> 

                    debug smtexpr 
                        "const_of_smtlib_token %s failed" 
                        (HString.string_of_hstring t)
                    in

                    (* Cannot convert to an expression *)
                    failwith "Invalid constant symbol in S-expression"

  in

  debug smtexpr 
      "const_of_smtlib_token %s is %a" 
      (HString.string_of_hstring t)
      pp_print_term res
  in

  res

(* Static hashconsed strings *)
let s_int = HString.mk_hstring "Int" 
let s_real = HString.mk_hstring "Real" 
let s_bool = HString.mk_hstring "Bool" 


(* Convert an S-expression to a sort *)
let type_of_smtlib_sexpr = 

  function 
    
    | HStringSExpr.Atom s when s == s_int -> Type.t_int
                                               
    | HStringSExpr.Atom s when s == s_real -> Type.t_real
                                                
    | HStringSExpr.Atom s when s == s_bool -> Type.t_bool 
                                                
    | HStringSExpr.Atom _
    | HStringSExpr.List _ as s -> 
      
      raise
        (Invalid_argument 
           (Format.asprintf 
              "Sort %a not supported" 
              HStringSExpr.pp_print_sexpr s))



(* Conversions for SMTLIB *)
let smtlib_string_sexpr_conv = 

  { s_let = HString.mk_hstring "let";
    s_forall = HString.mk_hstring "forall";
    s_exists = HString.mk_hstring "exists";
    s_div = HString.mk_hstring "/";
    s_minus = HString.mk_hstring "-";
    s_define_fun = HString.mk_hstring "define-fun";
    const_of_atom = const_of_smtlib_atom;
    symbol_of_atom = symbol_of_smtlib_atom;
    type_of_sexpr = type_of_smtlib_sexpr;
    expr_of_string_sexpr = gen_expr_of_string_sexpr';
    expr_or_lambda_of_string_sexpr = gen_expr_or_lambda_of_string_sexpr' }
 

(* Convert an S-expression in SMTLIB format to a term *)
let expr_of_string_sexpr = 
  gen_expr_of_string_sexpr smtlib_string_sexpr_conv

(* Convert an S-expression in SMTLIB format to a lambda abstraction *)
let expr_or_lambda_of_string_sexpr = 
  gen_expr_or_lambda_of_string_sexpr smtlib_string_sexpr_conv