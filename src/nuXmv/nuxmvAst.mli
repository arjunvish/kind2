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

(** Minimally simplified NuXmv abstract syntax tree

    The types in this module closely represent the abstract syntax of
    NuXmv. No type checking or simplification is performed when
    constructing the abstract syntax tree, this is done when passing the
    abstract syntax through {!NuxmvChecker}. 

    Some values are reserved for future use and will cause the
    translation to intermediate Lustre to fail.

    A NuXmv file is parsed into a {!module} list, where a
    module is composed of different {!elements}

    - state variable declaration [VAR]
    - internal variable definition [DEFINE]
    - assign constraints [ASSIGN]
    - init constraints [INIT]
    - transition constraints [TRANS]
    - invar property constraint [INVAR]
    - liveliness property specification [LTLSPEC]

    Almost all types are annotated with the position in the input file
    for better error reporting in the translation.

    @author Andrew West *)

(** {1 Types} *)

(** An identifier *)
type ident = string

(** A NuXmv Expression *)
(* This expr_type is for specifying what different operations are allowed in the 
    expression and encapsulates NuXmv expressions *)
type expr_type = 
    | LtlExpr of Position.t * nuxmv_expr
    | InvarExpr of Position.t * nuxmv_expr
    | NextExpr of Position.t * nuxmv_expr
    | SimpleExpr of Position.t * nuxmv_expr
    | ArrayExpr of Position.t * expr_type list

and nuxmv_expr = 
    | True of Position.t
    | False of Position.t
    | CInt of Position.t * Numeral.t
    | CFloat of Position.t * Decimal.t
    | Ident of Position.t * comp_ident
    | CRange of Position.t * Numeral.t * Numeral.t
    (* | Call of Position.t * comp_ident * nuxmv_expr list *)
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
    | InclExp of Position.t * nuxmv_expr * nuxmv_expr
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

(** A complex identifier for multiple modules *)
and comp_ident = 
    | CIdent of Position.t * ident
    | PerIdent of Position.t * comp_ident * ident
    (*  
        | BrackIdent of Position.t * comp_ident * expr_type
        | Self of Position.t 
    *)
(** {1 Elements} *)
(** State variable declaraion specified with simple type or as a module *)
type state_var_decl =
    | SimpleType of Position.t * ident * simple_type_spec
    | ModuleType of Position.t * ident * module_type_specifier

(** Module type specified by the module identifier and expression list of params *)
and module_type_specifier = 
    | ModuleTypeSpecifier of Position.t * ident * expr_type list


(** Simple NuXmv types *)
and simple_type_spec = 
    | Bool of Position.t
    | Int of Position.t
    | Real of Position.t
    | IntRange of Position.t * Numeral.t * Numeral.t
    | EnumType of Position.t * (enum_type_value) list 

and enum_type_value = 
    | ETId of Position.t * ident
    | ETCInt of Position.t * Numeral.t

(** Define declarations are either simple or an array 
    definition with an identifier and an expression type *)
type define_element = 
    | SimpleDef of Position.t * ident * expr_type
    | ArrayDef of Position.t * ident * expr_type

(** Assign constraint *)
type assign_const = 
    | InitAssign of Position.t * ident * expr_type 
    | NextAssign of Position.t * ident * expr_type 
    | Assign of Position.t * ident * expr_type 

(** {1 Module} *)
(** A NuXmv module of a module identifier, a parameter variable list, 
    and a list of module elements *)
type nuxmv_module = 
    | CustomModule of ident * ident list * module_element list

and module_element = 
    | StateVarDecl of Position.t * state_var_decl list
    | DefineDecl of Position.t * define_element list
    | AssignConst of Position.t * assign_const list
    | InitConst of Position.t * expr_type
    | TransConst of Position.t * expr_type
    | InvarConst of Position.t * expr_type 
    | LtlSpec of Position.t * expr_type

(** A NuXmv program as a list of module specifications *)
type t = nuxmv_module list

