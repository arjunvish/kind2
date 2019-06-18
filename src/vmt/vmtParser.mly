(* Copyright (c) 2019 by the Board of Trustees of the University of Iowa

   Licensed under the Apache License, Version 2.0 (the "License") you
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
%{

module A = VmtAst

let mk_pos = Position.create_position 
  
%}


%token <string> ID
%token <int> NUM
%token TRUE FALSE
%token LPAREN RPAREN
%token COLON
%token EXCL NEXT INIT TRANS INVARPROP LIVEPROP
%token DECLAREFUN DEFINEFUN DECLARESORT DEFINESORT
%token SETLOGIC SETOPTION ASSERT
%token EOF

(* Priorities and associativity of operators, lowest first *)

%start<VmtAst.t> program

%%

program: el = nonempty_list( expression ) EOF                               { el }

expression: LPAREN e = expression_body RPAREN                               { e }

expression_body:
    | DECLAREFUN id = ID LPAREN sl = list( sort ) RPAREN s = sort           { A.DeclareFun (mk_pos $startpos, id, sl, s) }                              
    | DEFINEFUN f = function_def                                            { f }
    | DECLARESORT i = ID n = NUM                                            { A.DeclareSort (mk_pos $startpos, i, n) }
    | DEFINESORT i = ID LPAREN idl = list(ID) RPAREN s = sort               { A.DefineSort (mk_pos $startpos, i, idl, s) }
    | SETLOGIC i = ID                                                       { A.SetLogic (mk_pos $startpos, i) }
    | SETOPTION COLON i = ID o = cust_option                                { A.SetOption (mk_pos $startpos, i, o) }
    | ASSERT t = term                                                       { A.Assert (mk_pos $startpos, t) }

function_def:
    | fun_id = ID LPAREN sl = list(sorted_var) RPAREN s = sort t = term     { A.DefineFun (mk_pos $startpos, fun_id, sl, s, t) }

term:
    | c = constant                                                          { c }
    | LPAREN op = ID tl = list (term) RPAREN                                { A.Operation (mk_pos $startpos, op, tl) }
    | LPAREN EXCL t = term a = attribute RPAREN                             { A.AttributeTerm (mk_pos $startpos, t, a) }

sorted_var:
    | LPAREN id = ID s = sort RPAREN                                        { A.SortedVar (mk_pos $startpos, id, s) }
    
sort: 
    | id = ID                                                               { A.Sort (mk_pos $startpos, id) }
    | LPAREN id = ID sort_list = nonempty_list(sort) RPAREN                 { A.MultiSort (mk_pos $startpos, id, sort_list) }

cust_option:
    | a = attribute                                                         { a }

attribute:
    | NEXT id = ID                                                          { A.NextName (mk_pos $startpos, id) }
    | INIT TRUE                                                             { A.InitTrue (mk_pos $startpos) }
    | TRANS TRUE                                                            { A.TransTrue (mk_pos $startpos) }
    | INVARPROP n = NUM                                                     { A.InvarProperty (mk_pos $startpos, n) }
    | LIVEPROP n = NUM                                                      { A.LiveProperty (mk_pos $startpos, n) }

constant: 
    | i = ID                                                                { A.Ident (mk_pos $startpos, i) }
    | int = NUM                                                             { A.Numeral (mk_pos $startpos, int) }
    | TRUE                                                                  { A.True (mk_pos $startpos) }
    | FALSE                                                                 { A.False (mk_pos $startpos) }