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

(** @author Andrew West *)

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

val fail_at_position_pt: Position.t -> string -> unit

val from_channel: in_channel -> (output, parse_error) result

val from_file: string -> (output, parse_error) result

val of_file : string -> VmtAst.t SubSystem.t