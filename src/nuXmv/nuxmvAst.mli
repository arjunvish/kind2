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

type nuxmv_expr = 
    | True of Position.t
    | False of Position.t
    | CInt of Position.t * int
    | CFloat of Position.t * float
    | Ident of Position.t * ident
    | CRange of Position.t * int * int
    | Call of Position.t * comp_ident * nuxmv_expr list
    | Not of Position.t * nuxmv_expr
    | And of Position.t * nuxmv_expr * nuxmv_expr
    | Or of Position.t * nuxmv_expr * nuxmv_expr
    | Xor of Position.t * nuxmv_expr * nuxmv_expr
    | Xnor of Position.t * nuxmv_expr * nuxmv_expr
    | Impl of Position.t * nuxmv_expr * nuxmv_expr
    | Equiv of Position.t * nuxmv_expr * nuxmv_expr
    | Eq of Position.t * nuxmv_expr * nuxmv_expr
    | NotEq of Position.t * nuxmv_expr * nuxmv_expr
    | Lt of Position.t * nuxmv_expr * nuxmv_expr
    | Lte of Position.t * nuxmv_expr * nuxmv_expr
    | Gt of Position.t * nuxmv_expr * nuxmv_expr
    | Gte of Position.t * nuxmv_expr * nuxmv_expr
    | Plus of Position.t * nuxmv_expr * nuxmv_expr
    | Uminus of Position.t * nuxmv_expr
    | Minus of Position.t * nuxmv_expr * nuxmv_expr
    | Multiply of Position.t * nuxmv_expr * nuxmv_expr
    | Divide of Position.t * nuxmv_expr * nuxmv_expr
    | Mod of Position.t * nuxmv_expr * nuxmv_expr
    | SetExp of Position.t * nuxmv_expr list
    | CaseExp of Position.t * (nuxmv_expr * nuxmv_expr) list
    | IfThenElseExp of Position.t * nuxmv_expr * nuxmv_expr * nuxmv_expr
    | NextExp of Position.t * nuxmv_expr
    | NextState of Position.t * nuxmv_expr
    | Globally of Position.t * nuxmv_expr
    | Finally of Position.t * nuxmv_expr
    | Until of Position.t * nuxmv_expr * nuxmv_expr
    | Releases of Position.t * nuxmv_expr * nuxmv_expr
    | PrevState of Position.t * nuxmv_expr
    | NotPrevStateNot of Position.t * nuxmv_expr
    | Historically of Position.t * nuxmv_expr
    | Once of Position.t * nuxmv_expr
    | Since of Position.t * nuxmv_expr * nuxmv_expr
    | Triggered of Position.t * nuxmv_expr * nuxmv_expr

and comp_ident = 
    | CIdent of Position.t * ident
    (* | PerIdent of Position.t * comp_ident * ident
    | BrackIdent of Position.t * comp_ident * expr_type
    | Self of Position.t *)
    
and expr_type = 
    | LtlExpr of Position.t * nuxmv_expr
    | NextExpr of Position.t * nuxmv_expr
    | SimpleExpr of Position.t * nuxmv_expr
    | ArrayExpr of Position.t * expr_type list

type enum_type_value = 
    | ETId of Position.t * ident
    | ETCInt of Position.t * int

type simple_type_spec = 
    | Bool of Position.t
    | Int of Position.t
    | Real of Position.t
    | IntRange of Position.t * int * int
    | EnumType of Position.t * (enum_type_value) list 

type module_type_specifier = 
    | ModuleTypeSpecifier of Position.t * ident * expr_type list

type state_var_decl =
    | SimpleType of Position.t * ident * simple_type_spec
    | ModuleType of Position.t * ident * module_type_specifier

type define_element = 
    | SimpleDef of Position.t * ident * expr_type
    | ArrayDef of Position.t * ident * expr_type

type assign_const = 
    | InitAssign of Position.t * comp_ident * expr_type 
    | NextAssign of Position.t * comp_ident * expr_type 
    | Assign of Position.t * comp_ident * expr_type 

type module_element = 
    | StateVarDecl of Position.t * state_var_decl list
    | DefineDecl of Position.t * define_element list
    | AssignConst of Position.t * assign_const list
    | TransConst of Position.t * expr_type
    | LtlSpec of Position.t * expr_type

type nuxmv_module = 
    | CustomModule of ident * ident list * module_element list

type t = nuxmv_module list

val print_program : t -> string

