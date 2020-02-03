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

(** Parse Vmt input into the internal Kind2 transitional system 

    An OCamllex lexer in {!VmtLexer} and a Menhir parser in
    {!VmtParser} take the input and produce a single {!VmtAst.t}
    value, which is a minimally processed representation of a Lustre
    AST.

    This AST is then checked over by a semantics checker and a type
    checker in {!VmtChecker}.

    The main function {!of_file} of this module returns a system for
    the analysis strategies that can be turned into an internal
    transition system {!TransSys} by using functions in
    {!VmtTransSys} with relevant parameters.

    The whole input file is parsed and type checked first, then one
    module is designated as the main node. The returned {!Subsystem.t}
    has this main node at the top, and all called nodes as
    children. Nodes that are in the input file, but not called by the
    main node are discarded. No further cone of influence reduction is
    peformed yet, this happens only in {!VmtTransSys} when the
    parameters of the analysis are known.


    In particular, the output of the entry point {!VmtParser.main}
    returns a Vmt file as a list of declarations of expressions.

    The function {!VmtChecker.check_eval} is called
    with this list of expression declarations as input and returns either
    the ast of the vmt file back or an error declaring what was wrong.

    Afterwards, the module {!VmtTransSys} turns 
    the list of nodes into a transition system {!TransSys.t} 
    by means of the functions {!VmtTransSys.trans_sys_of_nodes}.

    @author Andrew West
*)

type output = VmtAst.t

exception Parser_error

type parse_error =
  | UnexpectedChar of Position.t * char
  | SyntaxError of Position.t    
  | IdentifierAlreadyExists of Position.t * string
  | InvalidArgCount of Position.t * int * int
  | InvalidOperator of Position.t * string
  | InvalidType of Position.t * string
  | InvalidTypeWithOperator of Position.t * string * string
  | MissingAttribute of Position.t
  | MissingIdentifier of Position.t * string
  | MissingTerm of Position.t 
  | NonMatchingTypes of Position.t * string * string
  | NotSupported of Position.t * string

(* Method to translate a parse error from position type
  specified in the nuxmv path to that of the internal system*)
val fail_at_position_pt: Position.t -> string -> unit

(** Parse from a channel, returns the AST and the environment format. *)
val from_channel: in_channel -> (output, parse_error) result

(** Parse from a file, returns the AST and the environment format. *)
val from_file: string -> (output, parse_error) result

(** Parse from the file, returns the internal TransSys.t object 
for a nuxmv input file*)
val of_file : string -> VmtAst.t SubSystem.t