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
    | NextName of ident
    | InitTrue
    | TransTrue
    | InvarProperty of int
    | LiveProperty of int

type term = 
    | Ident of ident
    | Numeral of int
    | True
    | False
    | Operation of string * term list
    | AttributeTerm of term * attribute

type sort = 
    | Sort of string
    | MultiSort of string * sort list

type sorted_var = 
    | SortedVar of ident * sort

type vmt_expr = 
    | DeclareFun of ident * sort list * sort
    | DefineFun of ident * sorted_var list * sort * term
    | DeclareSort of ident * int
    | DefineSort of ident * ident list * sort
    | SetLogic of ident
    | SetOption of ident * attribute
    | Assert of term

type t = vmt_expr list