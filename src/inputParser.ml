(* This file is part of the Kind 2 model checker.

   Copyright (c) 2014 by the Board of Trustees of the University of Iowa

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

open Lib

(* Use the generic lexer *)
open Genlex

module I = LustreIdent
module E = LustreExpr

(* Keywords are the comma and the Boolean literals *)
let lexer = make_lexer [","; "true"; "false"]

(* Parse one line in CSV file *)
let rec parse_stream = parser
    
    (* A line starting with an identifier, followed by a comma and a
       sequence of values *)
  | [< 'Ident name; 'Kwd ","; sequence = parse_sequence >] ->
    
    try
    
      (* Find the state variable of top scope *) 
      let state_var = 
        E.state_var_of_ident I.top_scope_index (I.mk_string_ident name)
      in
      
      (* State variable must be an input *)
      if StateVar.is_input state_var then 
        
        (* Return state variable and its input *)
        (state_var, sequence)
        
      else
        
        (* Fail *)
        (Event.log
           `Interpreter
           Event.L_fatal
           "State variable %s is not an input" 
           name;
         
         raise (Invalid_argument "parse_stream"))
        
    with Not_found ->
      
      (* Fail *)
      (Event.log
         `Interpreter
         Event.L_fatal
         "State variable %s not found" 
         name;
       
       raise (Invalid_argument "parse_stream"))
      

(* Parse a sequence of values *)
and parse_sequence = parser

  (* Sequence starting with an integer *)
  | [< 'Int value; 
       int_sequence = 
         parse_int_sequence [Term.mk_num_of_int value] >] -> 

    int_sequence

(*
  (* Sequence starting with a float *)
  | [< 'Float value;
       float_sequence = 
         parse_float_sequence [Term.mk_dec_of_float value] >] ->

    float_sequence
*)

  (* Sequence starting with the Boolean value true *)
  | [< 'Kwd "true"; 
       bool_sequence = 
         parse_bool_sequence [Term.t_true] >] -> bool_sequence

  (* Sequence starting with the Boolean value false *)
  | [< 'Kwd "false"; 
       bool_sequence = 
         parse_bool_sequence [Term.t_false] >] -> 
  
  bool_sequence


(* Parse a sequence of integers *)
and parse_int_sequence l = parser

  (* Integer value with preceeding comma *)
  | [< 'Kwd ","; 
       'Int value; 
       int_sequence = 
         parse_int_sequence
           ((Term.mk_num_of_int value) :: l) >] -> 

    int_sequence

  (* End of the sequence *)
  | [< >] -> 

    (* Return list reversed *)
    List.rev l

(*

(* Parse a sequence of floats *)
and parse_float_sequence l = parser

  (* Integer value with preceeding comma *)
  | [< 'Kwd ","; 
       'Float value; 
       float_sequence = 
         parse_float_sequence
           ((Term.mk_dec_of_float value) :: l) >] -> 

    float_sequence

  (* End of a sequence *)
  | [< >] -> 

    (* Return list reversed *)
    List.rev l
*)


(* Parse a sequence of Booleans values *)
and parse_bool_sequence l  = parser

  (* Boolean value with preceding comma *)
  | [< 'Kwd ","; b = parse_bool_sequence_aux l >] -> b

  (* End of the sequence *)
  | [< >] -> List.rev l
                                
and parse_bool_sequence_aux l = parser

  (* True literal *)
  |[< 'Kwd "true"; 
      bool_sequence = parse_bool_sequence (Term.t_true :: l) >] -> 

    bool_sequence

  (* False literal *)
  | [< 'Kwd "false"; 
       bool_sequence = parse_bool_sequence (Term.t_false :: l) >] -> 

    bool_sequence

let parse s = 
  
  parse_stream (lexer (Stream.of_string s))

(* Read in a csv file *)
let read_file filename = 
  let chan = open_in filename in
  let rec parse_chan acc  = 
    try
      let line = input_line chan in
      parse_chan ((parse line)::acc)
    with End_of_file ->
      close_in chan; 
      acc
  in
  
  parse_chan []




(* 
   Local Variables:
   compile-command: "make -C .. -k"
   tuareg-interactive-program: "./kind2.top -I ./_build -I ./_build/SExpr"
   indent-tabs-mode: nil
   End: 
*)
