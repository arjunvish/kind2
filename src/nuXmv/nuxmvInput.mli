(* This file is part of the Kind 2 model checker.

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

(** Parse NuXmv input into the internal Kind2 transitional system 

    An OCamllex lexer in {!NuxmvLexer} and a Menhir parser in
    {!NuxmvParser} take the input and produce a sinlge {!NuxmvAst.t}
    value, which is a minimally processed representation of a Lustre
    AST.

    This AST is then checked over by a semantics checker and a type
    checker in {!NuxmvChecker}.

    The main function {!of_file} of this module returns a system for
    the analysis strategies that can be turned into an internal
    transition system {!TransSys} by using functions in
    {!LustreTransSys} with relevant parameters.

    The whole input file is parsed and type checked first, then one
    module is designated as the main node. The returned {!Subsystem.t}
    has this main node at the top, and all called nodes as
    children. Nodes that are in the input file, but not called by the
    main node are discarded. No further cone of influence reduction is
    peformed yet, this happens only in {!LustreTransSys} when the
    parameters of the analysis are known.

    The main node is chosen to be, in order of precedence:

    - the module with the name [MAIN]






    In particular, the output of the entry point {!NuxmvParser.main}
    returns a NuXmv file as a list of declarations of modules

    A module contains variable declarations, transitional assignments 
    for variables, property specifications, etc.

    The function {!NuxmvChecker.semantic_eval} is called
    with this list of declarations as input and returns either a 
    CheckOk or a CheckError depending on if a semantic was violated.
    Following this if passing the semantic check the function
    {!NuxmvChecker.type_eval} is called and the list plus the 
    environment layout of file is returned otherwise an error
     is returned.


    After passing both checks, the module {!NuxmvTransSys} turns 
    the list of nodes into a transition system {!TransSys.t} 
    by means of the functions {!NuxmvTransSys.trans_sys_of_nodes}.

    @author Andrew West
*)

type output = NuxmvAst.t * NuxmvChecker.env

exception Parser_error

type parse_error =
  | UnexpectedChar of Position.t * char
  | SyntaxError of Position.t
  | LtlUseError of Position.t
  | NextExprError of Position.t
  | DoubleNextExprError of Position.t
  | RangeLowerValueError of Position.t
  | ExpectedTypeError of Position.t * NuxmvChecker.nuxmv_ast_type list * NuxmvChecker.nuxmv_ast_type 
  | NonMatchingTypeError of Position.t * NuxmvChecker.nuxmv_ast_type * NuxmvChecker.nuxmv_ast_type
  | MissingVariableError of Position.t  * string
  | VariableAlreadyDefinedError of Position.t * string
  | EnumValueExistenceError of Position.t * string 
  | EnumNotContainValue of Position.t * string
  | MainModuleMissing of Position.t
  | MissingModule of Position.t * string
  | ModuleCalledTooManyArgs of Position.t * int * int
  | ModuleCalledMissingArgs of Position.t * int * int
  | AccessOperatorAppliedToNonModule of Position.t
  | MainModuleHasParams of Position.t 
  | NotSupportedError of Position.t * string

(* Method to translate a parse error from position type
  specified in the nuxmv path to that of the internal system*)
val fail_at_position_pt: Position.t -> string -> unit


(** Parse from a channel, returns the AST and the environment format. *)
val from_channel: in_channel -> (output, parse_error) result

(** Parse from the file, returns the AST and the environment format. *)
val from_file: string -> (output, parse_error) result


(** Parse from the file, returns the internal TransSys.t object 
for a nuxmv input file*)
val of_file : string -> (NuxmvAst.t * NuxmvChecker.env) SubSystem.t

