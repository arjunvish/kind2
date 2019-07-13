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

(** @author Daniel Larraz
    @author Andrew West *)

{
  module P = VmtParser

  exception Unexpected_Char of char
}

let whitespace = [' ' '\t']

let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']

let letter = ['a'-'z' 'A'-'Z']

let symbol_chars = ['$' '#' '+' '-' '/' '*' '=' '%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@']

let numeral = '0' | ['1'-'9'] (digit)*

let decimal = numeral '.' '0'* numeral

(* 
  let hexidecimal = '#''x' [digit 'a'-'f''A'-'F']+

  let binary = '#''b' ['0''1']+ 
*)

let simple_symbol = (letter | symbol_chars) (digit | letter | symbol_chars)*

let symbol = simple_symbol | '|' [^'|']* '|'

let index = numeral | symbol

let identifier = symbol | '(' '_' symbol index+ ')'

rule token = parse
(* Keywords *)
  | "declare-fun"     { P.DECLAREFUN }
  | "define-fun"      { P.DEFINEFUN }
  | "declare-sort"    { P.DECLARESORT }
  | "define-sort"     { P.DEFINESORT }
  | "set-option"      { P.SETOPTION }
  | "set-logic"       { P.SETLOGIC }
  | "assert"          { P.ASSERT }
  | "let"             { P.LET }
  | ":next"           { P.NEXT }
  | ":init"           { P.INIT }
  | ":trans"          { P.TRANS }
  | ":invar-property" { P.INVARPROP }
  | ":live-property"  { P.LIVEPROP }

  (* Punctuation *)
  | '('               { P.LPAREN }
  | ')'               { P.RPAREN }
  | ':'               { P.COLON }
  | '!'               { P.EXCL }

  (* Constants *)
  | "true"            { P.TRUE }
  | "false"           { P.FALSE }
  | numeral as int    { P.INT (int_of_string (int)) }
  | decimal as real   { P.REAL (float_of_string (real)) }
  | identifier as id  { P.ID id }

  (* Whitespace and New Line (both ignored) *)
  | whitespace        { token lexbuf }
  | newline           { Lexing.new_line lexbuf ; token lexbuf }

  (* Inline Comment *)
  | ';'               { skip_to_eol lexbuf }

  (* Skip set info command *)
  | "(set-info"       { skip_to_eol lexbuf }

  (* End of File *)
  | eof               { P.EOF }

  (* Unexpected Character *)
  | _ as c            { raise (Unexpected_Char c) }

and skip_to_eol = parse

  (* Count new line and resume *)
  | newline { Lexing.new_line lexbuf; token lexbuf } 

  (* Line ends at end of file *)
  | eof { token lexbuf }

  (* Ignore characters *)
  | _ { skip_to_eol lexbuf }



