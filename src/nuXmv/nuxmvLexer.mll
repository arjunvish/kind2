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
  module P = NuxmvParser

  exception Unexpected_Char of char
}

let whitespace = [' ' '\t']

let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']

let letter = ['_' 'a'-'z' 'A'-'Z']

let identifier = letter (digit | letter | ['$' '#' '-'])*

let constInt = '-'? (digit)+

let constReal = '-'? (digit)+ '.' (digit)+ | '-'? (digit)+ '.' (digit)+ 'e' '-'? (digit)+ | '-'? (digit)+ '.' (digit)+ 'E' '-'? (digit)+

let fractional = "f'" | "F'" 

rule token = parse
  | "MODULE"           { P.MODULE }
  | "VAR"              { P.VAR }
  | "DEFINE"           { P.DEFINE }
  | "ASSIGN"           { P.ASSIGN }
  | "TRANS"            { P.TRANS }
  | "INVARSPEC"        { P.INVAR }
  | "LTLSPEC"          { P.LTLSPEC }
 (* | "self"             { P.SELF } *)
  | "init"             { P.INIT }
  | "next"             { P.NEXT }
  | 'X'                { P.X }
  | 'G'                { P.G }
  | 'F'                { P.F }
  | 'U'                { P.U }
  | 'V'                { P.V }
  | 'Y'                { P.Y }
  | 'Z'                { P.Z }
  | 'H'                { P.H }
  | 'O'                { P.O }
  | 'S'                { P.S }
  | 'T'                { P.T }
  | "case"             { P.CASE }
  | "esac"             { P.ESAC }
  | "boolean"          { P.BOOL }
  | "integer"          { P.INT }
  | "real"             { P.REAL }
  | "TRUE"             { P.TRUE }
  | "FALSE"            { P.FALSE }
  | '.'                { P.PERIOD }
  | '?'                { P.THEN }
  | ".."               { P.DPERIOD }
  | ';'                { P.SEMICOLON }
  | ':'                { P.COLON }
  | '{'                { P.LCURLBRACK }
  | '}'                { P.RCURLBRACK }
  | '['                { P.LSQBRACK }
  | ']'                { P.RSQBRACK }
  | ','                { P.COMMA }
  | '('                { P.LPAREN }
  | ')'                { P.RPAREN }
  | '&'                { P.AND }
  | '|'                { P.OR }
  | "xor"              { P.XOR }
  | "xnor"             { P.XNOR }
  | '!'                { P.NOT }
  | "->"               { P.RARROW }
  | "<->"              { P.DARROW }
  | '='                { P.EQ }
  | "!="               { P.NEQ }
  | '<'                { P.LT }
  | '>'                { P.GT }
  | "<="               { P.LTE }
  | ">="               { P.GTE }
  | ":="               { P.ASSIGNMENT }
  | '+'                { P.PLUS }
  | '-'                { P.MINUS }
  | '*'                { P.MUL }
  | '/'                { P.DIV }
  | "mod"              { P.MOD }
  | fractional         { P.FRACTIONAL }
  | constInt as int    { P.CINT (int_of_string (int)) }
  | constReal as real  { P.CREAL (float_of_string (real)) }
  | identifier as id   { P.ID id }

  (* Whitespace and New Line (both ignored) *)
  | whitespace       { token lexbuf }
  | newline          { Lexing.new_line lexbuf ; token lexbuf }

  (* Inline.
    Need to have the '-'* here, otherwise "---" would be matched 
    as operator *)
  | "--" '-'* { skip_to_eol lexbuf }
  (* Multi-line.
  | "/*" { skip_commented_slashstar lexbuf }
  | "(*" { skip_commented_parenstar lexbuf }
  *)

  (* End of File *)
  | eof              { P.EOF }

  (* Unexpected Character *)
  | _ as c           { raise (Unexpected_Char c) }

and skip_to_eol = parse

  (* Count new line and resume *)
  | newline { Lexing.new_line lexbuf; token lexbuf } 

  (* Line ends at end of file *)
  | eof { token lexbuf }

  (* Ignore characters *)
  | _ { skip_to_eol lexbuf }



