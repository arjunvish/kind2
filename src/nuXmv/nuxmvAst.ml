(* Copyright (c) 2019 by the Board of Trustees of the University of Iowa

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

(* @author Andrew West*)

type ident = string


(* Basic Expressions *)
type nuxmv_expr = 
    (* Values *)
    | True of Position.t
    | False of Position.t
    | CInt of Position.t * int
    | CFloat of Position.t * float
    | Ident of Position.t * ident
    | CRange of Position.t * int * int

    (* Function Call *)
    | Call of Position.t * comp_ident * nuxmv_expr list

    (* Boolean operators *)
    | Not of Position.t * nuxmv_expr
    | And of Position.t * nuxmv_expr * nuxmv_expr
    | Or of Position.t * nuxmv_expr * nuxmv_expr
    | Xor of Position.t * nuxmv_expr * nuxmv_expr
    | Xnor of Position.t * nuxmv_expr * nuxmv_expr
    | Impl of Position.t * nuxmv_expr * nuxmv_expr
    | Equiv of Position.t * nuxmv_expr * nuxmv_expr

    (* Relations *)
    | Eq of Position.t * nuxmv_expr * nuxmv_expr
    | NotEq of Position.t * nuxmv_expr * nuxmv_expr
    | Lt of Position.t * nuxmv_expr * nuxmv_expr
    | Lte of Position.t * nuxmv_expr * nuxmv_expr
    | Gt of Position.t * nuxmv_expr * nuxmv_expr
    | Gte of Position.t * nuxmv_expr * nuxmv_expr

    (* Arithmetic operators *)
    | Plus of Position.t * nuxmv_expr * nuxmv_expr
    | Uminus of Position.t * nuxmv_expr
    | Minus of Position.t * nuxmv_expr * nuxmv_expr
    | Multiply of Position.t * nuxmv_expr * nuxmv_expr
    | Divide of Position.t * nuxmv_expr * nuxmv_expr
    | Mod of Position.t * nuxmv_expr * nuxmv_expr
    
    (* Set Expression *)
    | SetExp of Position.t * nuxmv_expr list

    (* Case Expression*)
    | CaseExp of Position.t * (nuxmv_expr * nuxmv_expr) list

    (* If-Then-Else Expression *)
    | IfThenElseExp of Position.t * nuxmv_expr * nuxmv_expr * nuxmv_expr

    (* Next Expression*)
    | NextExp of Position.t * nuxmv_expr
    (* Ltl Operations *)
        (* FUTURE *)
    | NextState of Position.t * nuxmv_expr
    | Globally of Position.t * nuxmv_expr
    | Finally of Position.t * nuxmv_expr
    | Until of Position.t * nuxmv_expr * nuxmv_expr
    | Releases of Position.t * nuxmv_expr * nuxmv_expr
        (* PAST *)
    | PrevState of Position.t * nuxmv_expr
    | NotPrevStateNot of Position.t * nuxmv_expr
    | Historically of Position.t * nuxmv_expr
    | Once of Position.t * nuxmv_expr
    | Since of Position.t * nuxmv_expr * nuxmv_expr
    | Triggered of Position.t * nuxmv_expr * nuxmv_expr

(* Complex Identifiers *)
and comp_ident = 
    | CIdent of Position.t * ident
    (* | PerIdent of Position.t * comp_ident * ident
    | BrackIdent of Position.t * comp_ident * expr_type (* These are more advanced than the exampls I have, so I will skip for now *)
    | Self of Position.t *)
    
and expr_type = 
    | LtlExpr of Position.t * nuxmv_expr
    | NextExpr of Position.t * nuxmv_expr
    | SimpleExpr of Position.t * nuxmv_expr
    (* Array Expression*)
    | ArrayExpr of Position.t * expr_type list

type enum_type_value = 
    | ETId of Position.t * ident
    | ETCInt of Position.t * int

type simple_type_spec = 
    | Bool of Position.t
    | Int of Position.t
    | Real of Position.t
    | IntRange of Position.t * int * int
    | EnumType of Position.t * (enum_type_value) list (* Assert that it is either an indent or cint*)

type module_type_specifier = 
    | ModuleTypeSpecifier of Position.t * ident * expr_type list

type state_var_decl =
    | SimpleType of Position.t * ident * simple_type_spec
    | ModuleType of Position.t * ident * module_type_specifier

type define_element = 
    | SimpleDef of Position.t * ident * expr_type (* Assert no next operation in expr *)
    | ArrayDef of Position.t * ident * expr_type (* Assert that it is an array expr and is allowed next *)

type assign_const = 
    | InitAssign of Position.t * comp_ident * expr_type (* Assert not next operation *)
    | NextAssign of Position.t * comp_ident * expr_type (* Assert not next operation *)
    | Assign of Position.t * comp_ident * expr_type (* Next operation allowed *)

type module_element = 
    | StateVarDecl of Position.t * state_var_decl list
    | DefineDecl of Position.t * define_element list
    | AssignConst of Position.t * assign_const list
    | TransConst of Position.t * expr_type (* Next operation is allowed *)
    | LtlSpec of Position.t * expr_type

type nuxmv_module = 
    | CustomModule of ident * ident list * module_element list

(* A nuxmv program *)
type t = nuxmv_module list

(* Pretty-Printing *)
(* let rec s_eval_expr (ltl: bool) (next:bool) (expr: A.nuxmv_expr) : semantic_error_type check_result = 
    match expr with
    | True _ -> CheckOk
    | False _ -> CheckOk
    | CInt _ -> CheckOk
    | CFloat _ -> CheckOk
    | Ident _ -> CheckOk
    | CRange (p, i1, i2) -> if i1 <= i2 then CheckOk else CheckError (RangeLowerValue (p))
    | A.Call (_, ci, nel) -> (let result1 = s_eval_complex_id ci in
                              match result1 with
                              | CheckError _ -> result1
                              | CheckOk -> let mapped2 = List.map (s_eval_expr ltl next) nel in
                                           let result2 = List.find_opt (fun x -> x != CheckOk) mapped2 in
                                           match result2 with
                                           | Some res -> res
                                           | None -> CheckOk (* How to deal with function call return types *) 
                            )
    | A.Not (_, e) -> s_eval_expr ltl next e
    | A.And (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Or (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Xor (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Xnor (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)   
    | A.Impl (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Equiv (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1) 
    | A.Eq (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.NotEq (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Lt (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Lte (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Gt (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Gte (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Plus (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Uminus (_, e) -> s_eval_expr ltl next e
    | A.Minus (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                             | CheckOk -> s_eval_expr ltl next e2
                             | r1 -> r1)
    | A.Multiply (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                                | CheckOk -> s_eval_expr ltl next e2
                                | r1 -> r1)
    | A.Divide (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                              | CheckOk -> s_eval_expr ltl next e2
                              | r1 -> r1)
    | A.Mod (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.SetExp (p, el) ->( if ltl then CheckError (LtlUse (p))
                           else
                            let mapped = List.map (s_eval_expr ltl false) el in
                            let result = List.find_opt (fun x -> x != CheckOk) mapped in
                            match result with
                            | Some res -> res
                            | None -> CheckOk )
    | A.CaseExp (p, el) -> (if ltl then CheckError (LtlUse (p))
                            else
                            let mapped1 = List.map (s_eval_expr ltl next) (List.map fst el) in
                            let result1 = List.find_opt (fun x -> x != CheckOk) mapped1 in
                            match result1 with
                            | Some res -> res
                            | None -> let mapped2 = List.map (s_eval_expr ltl next) (List.map snd el) in
                                      let result2 = List.find_opt (fun x -> x != CheckOk) mapped2 in
                                      match result2 with
                                      | Some res -> res
                                      | None -> CheckOk )
    | A.NextExp (p, e) -> if not next then CheckError (NextExpr (p))
                          else 
                            (match e with 
                             | A.NextExp _ -> CheckError (DoubleNextExpr (p))
                             | _ -> s_eval_expr ltl next e)
    | A.NextState (p, e) -> if not ltl then CheckError (LtlUse (p))
                            else s_eval_expr ltl next e
    | A.Globally (p, e) -> if not ltl then CheckError (LtlUse (p))
                           else s_eval_expr ltl next e 
    | A.Finally (p, e) -> if not ltl then CheckError (LtlUse (p))
                          else s_eval_expr ltl next e
    | A.Until (p, e1, e2) -> (if not ltl then CheckError (LtlUse (p))
                              else 
                                match s_eval_expr ltl next e1 with
                                | CheckOk -> s_eval_expr ltl next e2
                                | r1 -> r1)
    | A.Releases (p, e1, e2) -> (if not ltl then CheckError (LtlUse (p))
                                else 
                                match s_eval_expr ltl next e1 with
                                | CheckOk -> s_eval_expr ltl next e2
                                | r1 -> r1)
    | A.PrevState (p, e) -> if not ltl then CheckError (LtlUse (p))
                            else s_eval_expr ltl next e
    | A.NotPrevStateNot (p, e) -> if not ltl then CheckError (LtlUse (p))
                                  else s_eval_expr ltl next e
    | A.Historically (p, e) ->  if not ltl then CheckError (LtlUse (p))
                                else s_eval_expr ltl next e
    | A.Once (p, e) -> if not ltl then CheckError (LtlUse (p))
                       else s_eval_expr ltl next e
    | A.Since (p, e1, e2) -> if not ltl then CheckError (LtlUse (p))
                             else
                                (match s_eval_expr ltl next e1 with
                                | CheckOk -> s_eval_expr ltl next e2
                                | r1 -> r1)
    | A.Triggered (p, e1, e2) -> (if not ltl then CheckError (LtlUse (p))
                                  else 
                                 match s_eval_expr ltl next e1 with
                                 | CheckOk -> s_eval_expr ltl next e2
                                 | r1 -> r1)

and s_eval_expr_type (expr_type: A.expr_type) : semantic_error_type check_result = 
    match expr_type with
    | A.LtlExpr (_, expr) -> s_eval_expr true false expr
    | A.NextExpr (_, expr) -> s_eval_expr false true expr
    | A.SimpleExpr (_, expr) -> s_eval_expr false false expr
    | A.ArrayExpr (_, etl) -> let result_list = List.map s_eval_expr_type etl in
                              match List.find_opt (fun x -> x != CheckOk) result_list with
                              | None -> CheckOk
                              | Some e -> e

and s_eval_complex_id (ci: A.comp_ident):  semantic_error_type check_result =
    match ci with
    | CIdent _ -> CheckOk

let rec s_eval_state_var_decl (svdl: A.state_var_decl list) :  semantic_error_type check_result =
    match svdl with
    | [] -> CheckOk
    | svd :: t -> match svd with
                  | ModuleType (p, i, mts) -> 
                      (match mts with 
                          | ModuleTypeSpecifier (pos, id, etl) -> 
                          let mapped = List.map s_eval_expr_type etl in
                          let result = List.find_opt (fun x -> x != CheckOk) mapped in
                          match result with
                          | Some res -> res
                          | None -> s_eval_state_var_decl t )
                  | _ -> s_eval_state_var_decl t

let rec s_eval_define_decl (del: A.define_element list):  semantic_error_type check_result =
    match del with
    | [] -> CheckOk
    | svd :: t -> match svd with
                  | SimpleDef (pos, id, et) -> (match s_eval_expr_type et with
                                               | CheckOk -> s_eval_define_decl t
                                               | other -> other)
                  | ArrayDef (pos, id, et) -> (match s_eval_expr_type et with
                                               | CheckOk -> s_eval_define_decl t
                                               | other -> other)

let rec s_eval_assign_const (acl: A.assign_const list):  semantic_error_type check_result =
    match acl with
    | [] -> CheckOk
    | svd :: t -> match svd with
                  | InitAssign (pos, ci, et) -> (match s_eval_complex_id ci with
                                                | CheckOk -> s_eval_expr_type et
                                                | other -> other)
                  | NextAssign (pos, ci, et) -> (match s_eval_complex_id ci with
                                                | CheckOk -> s_eval_expr_type et
                                                | other -> other)
                  | Assign (pos, ci, et) -> (match s_eval_complex_id ci with
                                                | CheckOk -> s_eval_expr_type et
                                                | other -> other)

let s_eval_module_element (me : A.module_element): semantic_error_type check_result = 
    match me with
    | A.StateVarDecl (_, svdl) -> s_eval_state_var_decl svdl
    | A.DefineDecl (_, del) -> s_eval_define_decl del
    | A.AssignConst (_, acl) -> s_eval_assign_const acl
    | A.TransConst (_, expr_type) -> s_eval_expr_type expr_type
    | A.LtlSpec (_, expr_type) -> s_eval_expr_type expr_type*)
    
let print_enum_type_value (s: string) (etv: enum_type_value) : string =
    match etv with
    | ETId (_, id) -> s ^ id ^ "," 
    | ETCInt (_, i) -> s ^ (string_of_int i) ^ ","

let print_simple_type_spec (sts : simple_type_spec) : string =
    match sts with
    | Int _ -> "Int"
    | Bool _ -> "Bool"
    | Real _ -> "Real"
    | IntRange (_, i1, i2) -> (string_of_int i1) ^ ".." ^ (string_of_int i2)
    | EnumType (_, etvl) -> (List.fold_left print_enum_type_value "[" etvl) ^ "]"

let print_state_var_decl (s: string) (svd : state_var_decl) : string =
    match svd with
    | ModuleType _ -> s
    | SimpleType (_, id, sts) -> s ^ id ^ ":" ^ (print_simple_type_spec sts) ^ "\n"

let print_module_element (s:string) (me : module_element) : string =
    match me with
    | StateVarDecl (_, svdl) -> let newS = s ^ "VAR" ^ "\n" in 
                                (List.fold_left print_state_var_decl newS svdl)
    | DefineDecl (_, del) -> s
    | AssignConst (_, acl) -> s
    | TransConst (_, expr_type) -> s
    | LtlSpec (_, expr_type) -> s

let print_nuxmv_module (s: string) (nm : nuxmv_module) : string = 
    match nm with
    | CustomModule (_, idl, mel) -> List.fold_left print_module_element s mel

let print_program (program : t) : string = List.fold_left print_nuxmv_module "" program



