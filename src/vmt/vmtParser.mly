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

(** @author Andrew West*)
%{

module A = VmtAst

let mk_pos = Position.create_position 
  
%}


%token <string> ID
%token <Numeral.t> INT
%token <Decimal.t> REAL
%token TRUE FALSE
%token <Numeral.t> BVCONST
%token BOOLT INTT REALT BITVECT UNDERSCORE
%token LPAREN RPAREN
%token COLON
%token EXCL NEXT INIT TRANS INVARPROP LIVEPROP LET EXTRACT
%token DECLAREFUN DEFINEFUN DECLARESORT DEFINESORT
%token SETLOGIC SETOPTION ASSERT
%token EOF

(* Priorities and associativity of operators, lowest first *)

%start<VmtAst.t> program

%%

program: 
    | el = expression+ EOF
        { el }

expression: 
    | LPAREN e = expression_body RPAREN
        { e }

expression_body:
    | DECLAREFUN id = ID LPAREN sl = sort* RPAREN s = sort
        { A.DeclareFun (mk_pos $startpos, id, sl, s) }
    | DEFINEFUN f = function_def
        { f }
    | DECLARESORT i = ID n = INT
        { A.DeclareSort (mk_pos $startpos, i, n) }
    | DEFINESORT i = ID LPAREN idl = ID* RPAREN s = sort
        { A.DefineSort (mk_pos $startpos, i, idl, s) }
    | SETLOGIC i = ID
        { A.SetLogic (mk_pos $startpos, i) }
    | SETOPTION COLON i = ID o = cust_option
        { A.SetOption (mk_pos $startpos, i, o) }
    | ASSERT t = term
        { A.Assert (mk_pos $startpos, t) }

function_def:
    | fun_id = ID LPAREN sl = sorted_var* RPAREN s = sort t = term
        { A.DefineFun (mk_pos $startpos, fun_id, sl, s, t) }

term:
    | c = constant
        { c }
    | LPAREN op = ID tl = term* RPAREN
        { A.Operation (mk_pos $startpos, op, tl) }
    | LPAREN LPAREN UNDERSCORE EXTRACT finish = INT start = INT RPAREN t = term RPAREN
        { A.ExtractOperation (mk_pos $startpos, start, finish, t) }
    | LPAREN LET LPAREN vbl = var_binding+ RPAREN t = term RPAREN
        { A.Let (mk_pos $startpos, vbl, t) } 
    | LPAREN EXCL t = term al = attribute+ RPAREN
        { A.AttributeTerm (mk_pos $startpos, t, al) }

var_binding:
    | LPAREN id = ID t = term RPAREN
        { A.VarBind (mk_pos $startpos, id, t) }

sorted_var:
    | LPAREN id = ID s = sort RPAREN    
        { A.SortedVar (mk_pos $startpos, id, s) }
    
sort: 
    | id = ID
        { A.AmbiguousType (mk_pos $startpos, id) }
    | INTT
        { A.IntType (mk_pos $startpos) }
    | BOOLT
        { A.BoolType (mk_pos $startpos) }
    | REALT
        { A.RealType (mk_pos $startpos) }
    | LPAREN UNDERSCORE BITVECT i = INT RPAREN
        { A.BitVecType (mk_pos $startpos, i) }        
    | LPAREN s = sort sort_list = sort+ RPAREN
        { A.MultiSort (mk_pos $startpos, s, sort_list) }

cust_option:
    | a = attribute
        { a }

attribute:
    | NEXT id = ID
        { A.NextName (mk_pos $startpos, id) }
    | INIT TRUE
        { A.InitTrue (mk_pos $startpos) }
    | TRANS TRUE
        { A.TransTrue (mk_pos $startpos) }
    | INVARPROP n = INT
        { A.InvarProperty (mk_pos $startpos, n) }
    | LIVEPROP n = INT
        { A.LiveProperty (mk_pos $startpos, n) }

constant: 
    | i = ID
        { A.Ident (mk_pos $startpos, i) }
    | int = INT
        { A.Integer (mk_pos $startpos, int) }
    | real = REAL
        { A.Real (mk_pos $startpos, real) }
    | TRUE
        { A.True (mk_pos $startpos) }
    | FALSE
        { A.False (mk_pos $startpos) }
    | LPAREN UNDERSCORE v = BVCONST size = INT RPAREN
        { A.BitVecConst (mk_pos $startpos, v, size) }