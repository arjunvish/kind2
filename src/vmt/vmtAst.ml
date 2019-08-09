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

type attribute = 
    | NextName of Position.t * ident
    | InitTrue of Position.t
    | TransTrue of Position.t
    | InvarProperty of Position.t * Numeral.t
    | LiveProperty of Position.t * Numeral.t

type param = 
    | VarBind of Position.t * string * term

and term = 
    | Ident of Position.t * ident
    | Integer of Position.t * Numeral.t
    | Real of Position.t * Decimal.t
    | True of Position.t
    | False of Position.t
    | BitVecConst of Position.t * Numeral.t * Numeral.t
    | Operation of Position.t * string * term list
    | ExtractOperation of Position.t * Numeral.t * Numeral.t * term
    | AttributeTerm of Position.t * term * attribute list
    | Let of Position.t * param list * term

type sort = 
    | BoolType of Position.t
    | IntType of Position.t
    | RealType of Position.t
    | BitVecType of Position.t * Numeral.t
    | AmbiguousType of Position.t * string
    | MultiSort of Position.t * sort * sort list

type sorted_var = 
    | SortedVar of Position.t * ident * sort

type vmt_expr = 
    | DeclareFun of Position.t * ident * sort list * sort
    | DefineFun of Position.t * ident * sorted_var list * sort * term
    | DeclareSort of Position.t * ident * Numeral.t
    | DefineSort of Position.t * ident * ident list * sort
    | SetLogic of Position.t * ident
    | SetOption of Position.t * ident * attribute
    | Assert of Position.t * term

type t = vmt_expr list