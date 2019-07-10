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

module A = NuxmvAst

let mk_pos = Position.create_position 

exception Ltl_Use_Error
  
%}


%token MODULE VAR DEFINE ASSIGN TRANS INVAR LTLSPEC
%token X G F U V Y Z H O S T
%token <string> ID
%token <int> CINT
%token <float> CREAL
%token FRACTIONAL
%token BOOL INT REAL
%token TRUE FALSE
%token EQ NEQ LT GT LTE GTE THEN
%token RARROW DARROW
%token AND NOT OR XOR XNOR
%token PLUS MINUS MOD MUL DIV
%token NEXT INIT CASE ESAC
(* %token SELF *)
%token ASSIGNMENT
%token LPAREN RPAREN LCURLBRACK RCURLBRACK LSQBRACK RSQBRACK
%token COMMA SEMICOLON COLON PERIOD DPERIOD
%token EOF

(* Priorities and associativity of operators, lowest first *)
%right RARROW
%left  DARROW
%left  THEN COLON
%left  OR XOR XNOR
%left  AND
%left  EQ NEQ LT GT LTE GTE
%left  PLUS MINUS
%left  MUL DIV MOD
%left  NOT
%nonassoc X G F U V Y Z H O S T

%start<NuxmvAst.t> program
%type<NuxmvAst.expr_type> ltl_expr next_expr simple_expr

%%
program: ml = nonempty_list(module_decl) EOF { ml }

module_decl: MODULE id = ID mp = option(module_params) mb = option(module_body) { match (mp,mb) with 
                                                                                  | (None, None)     -> A.CustomModule (id, [], [])
                                                                                  | (Some p, None)   -> A.CustomModule (id, p, [])
                                                                                  | (None, Some b)   -> A.CustomModule (id, [], b)
                                                                                  | (Some p, Some b) -> A.CustomModule (id, p, b) }  

module_params: LPAREN mpl = separated_nonempty_list(COMMA, ID) RPAREN { mpl } 

module_body: mel = nonempty_list(module_element) { mel }

module_element:
   svd = state_var_declaration { svd }
 | dd = define_declaration { dd }
 | ac = assign_constraint { ac }
 | tc = trans_constraint { tc }
 | ivs = invar_specification { ivs }
 | ltls = ltl_specification { ltls }
 

(* State Var Declarations *)
state_var_declaration: VAR sve = nonempty_list(state_var_element) { A.StateVarDecl (mk_pos $startpos, sve) } ;

state_var_element:
    | i = ID COLON sts = simple_type_specifier SEMICOLON { A.SimpleType (mk_pos $startpos, i, sts) }
    | i = ID COLON mts = module_type_spec SEMICOLON { A.ModuleType (mk_pos $startpos, i, mts) }
    ;

simple_type_specifier:
    | BOOL { A.Bool (mk_pos $startpos) }
    | INT { A.Int (mk_pos $startpos) }
    | REAL { A.Real (mk_pos $startpos) }
    | i1 = CINT DPERIOD i2 = CINT { A.IntRange (mk_pos $startpos, i1, i2) }
    | LCURLBRACK etvl = separated_nonempty_list(COMMA, enum_type_value) RCURLBRACK { A.EnumType (mk_pos $startpos, etvl) }
    ;

enum_type_value:
    | i = ID { A.ETId (mk_pos $startpos, i) }
    | ci = CINT { A.ETCInt (mk_pos $startpos, ci) }
    ;

module_type_spec:
    | id = ID pl = option(module_type_spec_param_list) { match pl with 
                                                         | None -> A.ModuleTypeSpecifier (mk_pos $startpos, id, [])
                                                         | Some p -> A.ModuleTypeSpecifier (mk_pos $startpos, id, p) }
    ;

module_type_spec_param_list:
    | LPAREN RPAREN { [] }
    | LPAREN il = basic_expr_list RPAREN { il }
    ;

(* Definition Declarations *)
define_declaration: DEFINE del = nonempty_list(define_element) { A.DefineDecl (mk_pos $startpos, del)}
    ;

define_element:
    | i = ID ASSIGNMENT e = simple_expr SEMICOLON { A.SimpleDef (mk_pos $startpos, i, e) }
    | i = ID ASSIGNMENT ae = array_expr SEMICOLON { A.ArrayDef (mk_pos $startpos, i, ae) }
    ;

(* Assign Constraints *)
assign_constraint: ASSIGN ael = nonempty_list(assign_element) { A.AssignConst (mk_pos $startpos, ael) }
    ;

assign_element:
    | INIT LPAREN id = ID RPAREN ASSIGNMENT e = simple_expr SEMICOLON { A.InitAssign (mk_pos $startpos, id, e) }
    | NEXT LPAREN id = ID RPAREN ASSIGNMENT e = simple_expr SEMICOLON { A.NextAssign (mk_pos $startpos, id, e) }
    | id = ID ASSIGNMENT e = next_expr { A.Assign (mk_pos $startpos, id, e) }
    ;


(* Trans Constraints *)
trans_constraint: TRANS e = next_expr option(SEMICOLON) { A.TransConst (mk_pos $startpos, e) } 
    ;

(* Invar Specifications *)
invar_specification: INVAR e = invar_expr SEMICOLON { A.InvarSpec (mk_pos $startpos, e) }
    ;

(* LTL Specifications *)
ltl_specification: LTLSPEC le = ltl_expr option(SEMICOLON) { A.LtlSpec (mk_pos $startpos, le) }
    ;

(* Expression Types *)
%inline invar_expr:
    | e = expr { A.InvarExpr(mk_pos $startpos, e) }
    ;

%inline ltl_expr:
    | e = expr { A.LtlExpr(mk_pos $startpos, e) }
    ;

%inline simple_expr: (* Note that simple expressions cannot contain next operators *)
    | b = expr { A.SimpleExpr(mk_pos $startpos, b) }
    ;

%inline next_expr:
    | b = expr { A.NextExpr(mk_pos $startpos, b) }
    ;


complex_indentifier:
    | i = ID { A.CIdent (mk_pos $startpos, i) }
    | ci = complex_indentifier PERIOD i = ID { A.PerIdent (mk_pos $startpos, ci, i) }
    (* 
        | ci = complex_indentifier LSQBRACK e = simple_expr RSQBRACK { BrackIdent (mk_pos $startpos, ci, e) }
        | SELF { A.Self (mk_pos $startpos) } 
    *)
    ;

(* General Purpose Rules *)
expr:
    | c = constant { c }
    (* | f = function_call { f } *)
    | NOT e = expr { A.Not (mk_pos $startpos, e) }
    | e1 = expr AND e2 = expr { A.And (mk_pos $startpos, e1, e2) }
    | e1 = expr OR e2 = expr { A.Or (mk_pos $startpos, e1, e2) }
    | e1 = expr XOR e2 = expr { A.Xor (mk_pos $startpos, e1, e2) }
    | e1 = expr XNOR e2 = expr { A.Xnor (mk_pos $startpos, e1, e2) }
    | e1 = expr RARROW e2 = expr { A.Impl (mk_pos $startpos, e1, e2) }
    | e1 = expr DARROW e2 = expr { A.Equiv (mk_pos $startpos, e1, e2) }
    | e1 = expr EQ e2 = expr { A.Eq (mk_pos $startpos, e1, e2)}
    | e1 = expr NEQ e2 = expr { A.NotEq (mk_pos $startpos, e1, e2) }
    | e1 = expr LT e2 = expr { A.Lt (mk_pos $startpos, e1, e2) }
    | e1 = expr GT e2 = expr { A.Gt (mk_pos $startpos, e1, e2) }
    | e1 = expr LTE e2 = expr { A.Lte (mk_pos $startpos, e1, e2) }
    | e1 = expr GTE e2 = expr { A.Gte (mk_pos $startpos, e1, e2) }
    | MINUS e = expr { A.Uminus (mk_pos $startpos, e)}
    | e1 = expr PLUS e2 = expr { A.Plus (mk_pos $startpos, e1, e2)}
    | e1 = expr MINUS e2 = expr { A.Minus (mk_pos $startpos, e1, e2) }
    | e1 = expr MUL e2 = expr { A.Multiply (mk_pos $startpos, e1, e2)}
    | e1 = expr DIV e2 = expr { A.Divide (mk_pos $startpos, e1, e2) }
    | e1 = expr MOD e2 = expr { A.Mod (mk_pos $startpos, e1, e2) }
    | LPAREN e = expr RPAREN { e }
    | LCURLBRACK el = separated_nonempty_list(COMMA, expr) RCURLBRACK { A.SetExp (mk_pos $startpos, el) }
    | CASE cel = nonempty_list(case_element) ESAC { A.CaseExp (mk_pos $startpos, cel) }
    | e1 = expr THEN e2 = expr COLON e3 = expr { A.IfThenElseExp (mk_pos $startpos, e1, e2, e3) }
    | NEXT LPAREN e = expr RPAREN { A.NextExp (mk_pos $startpos, e)}
    
    (* Ltl Expressions*)
        (* FUTURE *)
    | X le = expr { A.NextState (mk_pos $startpos, le) }
    | G le = expr { A.Globally (mk_pos $startpos, le) }
    | F le = expr { A.Finally (mk_pos $startpos, le) }
    | le1 = expr U le2 = expr { A.Until (mk_pos $startpos, le1, le2) }
    | le1 = expr V le2 = expr { A.Releases (mk_pos $startpos, le1, le2) }
        (* PAST *)
    | Y le = expr { A.PrevState (mk_pos $startpos, le) }
    | Z le = expr { A.NotPrevStateNot (mk_pos $startpos, le) }
    | H le = expr { A.Historically (mk_pos $startpos, le) }
    | O le = expr { A.Once (mk_pos $startpos, le) }
    | le1 = expr S le2 = expr { A.Since (mk_pos $startpos, le1, le2) }
    | le1 = expr T le2 = expr { A.Triggered (mk_pos $startpos, le1, le2) }
    ;

case_element:
    | e1 = expr COLON e2 = expr SEMICOLON { (e1, e2) }
    ;

(* function_call: 
    | ci = complex_indentifier LPAREN el = function_call_params RPAREN { A.Call (mk_pos $startpos, ci, el) }
    ;

function_call_params:
    | e = expr { [e] }
    | e = expr COMMA p = function_call_params  {e :: p}
    ; *)

basic_expr_list:
    | el = separated_nonempty_list(COMMA, simple_expr) { el }
    ;

array_expr:
    | LSQBRACK el = separated_nonempty_list(COMMA, next_expr) RSQBRACK { A.ArrayExpr (mk_pos $startpos, el) }
    | LSQBRACK al =  separated_nonempty_list(COMMA, array_expr) RSQBRACK { A.ArrayExpr (mk_pos $startpos, al) }
    ;

constant:
    | FALSE { A.False (mk_pos $startpos) }
    | TRUE { A.True (mk_pos $startpos) }
    | ci = complex_indentifier { A.Ident (mk_pos $startpos, ci) }
    | i = CINT { A.CInt (mk_pos $startpos, i) }
    | r = CREAL { A.CFloat (mk_pos $startpos, r) }
    | FRACTIONAL c1 = CINT DIV c2 = CINT { let f1 = float_of_int (c1) in
                                           let f2 = float_of_int (c2) in
                                                A.CFloat (mk_pos $startpos, (f1 /. f2)) }
    | i1 = CINT DPERIOD i2 = CINT { A.CRange (mk_pos $startpos, i1, i2) }
    ;
