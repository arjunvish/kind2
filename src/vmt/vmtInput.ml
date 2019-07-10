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

let parse_buffer lexbuf : (output, parse_error) result =
  try
    let check_res = VmtParser.program VmtLexer.token lexbuf |> VmtChecker.check_vmt in
    match check_res with
    | Ok abs_syn -> Ok abs_syn
    | Error error -> ( 
      let err = 
        match error with
        | VmtChecker.IdentifierAlreadyExists (pos, str) -> IdentifierAlreadyExists (pos, str)
        | VmtChecker.InvalidArgCount (pos, i1, i2) -> InvalidArgCount (pos, i1, i2)
        | VmtChecker.InvalidOperator (pos, str) -> InvalidOperator (pos, str)
        | VmtChecker.InvalidType (pos, str) -> InvalidType (pos, str)
        | VmtChecker.InvalidTypeWithOperator (pos, str1, str2) -> InvalidTypeWithOperator (pos, str1, str2)
        | VmtChecker.MissingAttribute pos -> MissingAttribute pos
        | VmtChecker.MissingIdentifier (pos, str) -> MissingIdentifier (pos, str)
        | VmtChecker.MissingTerm pos -> MissingTerm pos
        | VmtChecker.NonMatchingTypes (pos, str1, str2) -> NonMatchingTypes (pos, str1, str2)
        | VmtChecker.NotSupported (pos, str) -> NotSupported (pos, str)
      in
      Error err
    )
  with 
  | VmtLexer.Unexpected_Char c ->
    let pos = Position.get_position lexbuf in Error (UnexpectedChar (pos, c))
  | VmtParser.Error ->
    let pos = Position.get_position lexbuf in Error (SyntaxError pos)


let from_channel in_ch =
  parse_buffer (Lexing.from_channel in_ch)


let from_file filename =
  let in_ch = open_in filename in
  let lexbuf = Lexing.from_channel in_ch in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with
                                Lexing.pos_fname = filename };
  parse_buffer lexbuf

let of_file filename = 
  match from_file filename with
  | Ok res -> { SubSystem.scope = [] ; source = res ; has_contract = false ; has_modes = false ; has_impl = false ; subsystems = [] }
  | Error _ -> failwith "NuXmv parsing/semantic check/type check error."
