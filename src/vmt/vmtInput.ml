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

let fail_at_position_pt pos msg =
  Log.log Lib.L_error "Parser error at %a: @[<v>%s@]"
    Position.pp_print_position pos msg

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
        | VmtChecker.InvalidTypeWithOperator (pos, str1, str2) -> 
            InvalidTypeWithOperator (pos, str1, str2)
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
  | Ok res -> { SubSystem.scope = ["Main"] ; source = res ; has_contract = false ; 
                has_modes = false ; has_impl = false ; subsystems = [] }
  | Error error ->(
    match error with
    | UnexpectedChar (pos, char) -> (
      fail_at_position_pt pos 
        ("unexpected character ‘"^ (String.make 1 char) ^ "’") 
      ; raise (Parser_error)
    )
    | SyntaxError (pos) -> (
      fail_at_position_pt pos "syntax error" 
      ; raise (Parser_error)
    )
    | IdentifierAlreadyExists (pos, str) -> (
      fail_at_position_pt pos 
        ("Identifier " ^ str ^ " already exists in the scope")
      ; raise (Parser_error)
    )
    | InvalidArgCount (pos, i1, i2) -> (
      fail_at_position_pt pos ("Invalid argument count 
        ("^string_of_int i2^"given but "^string_of_int i1^" expected)")
      ; raise (Parser_error)
    )
    | InvalidOperator (pos, str) -> (
      fail_at_position_pt pos ("Invalid operator '"^str^"'")
      ; raise (Parser_error)
    )
    | InvalidType (pos, str) -> (
      fail_at_position_pt pos ("Invalid type '"^str^"'")
      ; raise (Parser_error)
    )
    | InvalidTypeWithOperator (pos, str1, str2) -> (
      fail_at_position_pt pos 
        ("Operator '"^str2^"' doesn't support type '"^str1^"'")
      ; raise (Parser_error)
    )
    | MissingAttribute pos -> (
      fail_at_position_pt pos 
        ("Attribute is required when using (! term attribute_list)")
      ; raise (Parser_error)
    )
    | MissingIdentifier (pos, str) -> (
      fail_at_position_pt pos ("Identifier '"^str^"' is missing")
      ; raise (Parser_error)
    )
    | MissingTerm pos -> (
      fail_at_position_pt pos ("Term is required")
      ; raise (Parser_error)
    )
    | NonMatchingTypes (pos, str1, str2) -> (
      fail_at_position_pt pos 
        ("Types '"^str1^"' and '"^str2^"' don't match in the expression")
      ; raise (Parser_error)
    )
    | NotSupported (pos, str) -> (
      fail_at_position_pt pos 
        ("Functionality '"^str^"' is parable but not supported")
      ; raise (Parser_error)
    )
  ) 
