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
    | InvarProperty of Position.t * int
    | LiveProperty of Position.t * int

type term = 
    | Ident of Position.t * ident
    | Numeral of Position.t * int
    | True of Position.t
    | False of Position.t
    | Operation of Position.t * string * term list
    | AttributeTerm of Position.t * term * attribute

type sort = 
    | Sort of Position.t * string
    | MultiSort of Position.t * string * sort list

type sorted_var = 
    | SortedVar of Position.t * ident * sort

type vmt_expr = 
    | DeclareFun of Position.t * ident * sort list * sort
    | DefineFun of Position.t * ident * sorted_var list * sort * term
    | DeclareSort of Position.t * ident * int
    | DefineSort of Position.t * ident * ident list * sort
    | SetLogic of Position.t * ident
    | SetOption of Position.t * ident * attribute
    | Assert of Position.t * term

type t = vmt_expr list