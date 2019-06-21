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


let parse_buffer lexbuf : (output, parse_error) result =
  try
    let abs_syn = VmtParser.program VmtLexer.token lexbuf in
    Ok abs_syn
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
