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

(** @author Daniel Larraz *)

type output = NuxmvAst.t

exception Parser_error

type parse_error =
  | UnexpectedChar of Position.t * char
  | SyntaxError of Position.t
  | LtlUseError of Position.t
  | NextExprError of Position.t
  | DoubleNextExprError of Position.t
  | RangeLowerValueError of Position.t
  | ExpectedTypeError of Position.t * NuxmvChecker.nuxmv_ast_type list * NuxmvChecker.nuxmv_ast_type 
  | NonMatchingTypeError of Position.t * NuxmvChecker.nuxmv_ast_type * NuxmvChecker.nuxmv_ast_type 
  | MissingVariableError of Position.t *string 
  | VariableAlreadyDefinedError of Position.t * string
  | EnumValueExistenceError of Position.t *string 
  | EnumNotContainValue of Position.t * string 
  | MainModuleMissing of Position.t
  | MissingModule of Position.t * string 
  | ModuleCalledTooManyArgs of Position.t * int * int 
  | ModuleCalledMissingArgs of Position.t * int * int
  | AccessOperatorAppliedToNonModule of Position.t
  | MainModuleHasParams of Position.t
  | NotSupportedError of Position.t * string

let fail_at_position_pt pos msg =
  Log.log Lib.L_error "Parser error at %a: @[<v>%s@]"
    Position.pp_print_position pos msg

let parse_buffer lexbuf : (output, parse_error) result =
  try
    let abstract_syntax = NuxmvParser.program NuxmvLexer.token lexbuf in
        match NuxmvChecker.semantic_eval abstract_syntax with
        | NuxmvChecker.CheckError (NuxmvChecker.LtlUse pos )-> Error (LtlUseError pos)
        | NuxmvChecker.CheckError (NuxmvChecker.NextExpr pos ) -> Error (NextExprError pos)
        | NuxmvChecker.CheckError (NuxmvChecker.DoubleNextExpr pos ) -> Error (DoubleNextExprError pos)
        | NuxmvChecker.CheckError (NuxmvChecker.RangeLowerValue pos ) -> Error (RangeLowerValueError pos)
        | NuxmvChecker.CheckError (NuxmvChecker.NotSupported (pos, str) ) -> Error (NotSupportedError (pos, str))
        | NuxmvChecker.CheckOk -> (
          let type_res = NuxmvChecker.type_eval abstract_syntax in 
            match type_res with
            | Error (NuxmvChecker.Expected (pos, tl, t )) -> Error (ExpectedTypeError (pos, tl, t))
            | Error (NuxmvChecker.NonMatching (pos, t1, t2) ) -> Error (NonMatchingTypeError (pos, t1, t2) )
            | Error (NuxmvChecker.MissingVariable (pos, str) ) -> Error (MissingVariableError (pos, str))
            | Error (NuxmvChecker.VariableAlreadyDefined (pos, str) ) -> Error (VariableAlreadyDefinedError (pos, str))
            | Error (NuxmvChecker.EnumValueExist (pos, str) ) -> Error (EnumValueExistenceError (pos, str))
            | Error (NuxmvChecker.EnumNotContain (pos, str) ) -> Error (EnumNotContainValue (pos, str))
            | Error (NuxmvChecker.MainError pos )-> Error (MainModuleMissing pos)
            | Error (NuxmvChecker.MissingModule (pos, str) ) -> Error (MissingModule (pos, str))
            | Error (NuxmvChecker.ModuleCallTooMany (pos, i1, i2) ) -> Error (ModuleCalledTooManyArgs (pos, i1, i2) )
            | Error (NuxmvChecker.ModuleCallMissing (pos, i1, i2)  ) -> Error (ModuleCalledMissingArgs (pos, i1, i2) )
            | Error (NuxmvChecker.AccessOperatorAppliedToNonModule pos)  -> Error (AccessOperatorAppliedToNonModule pos)
            | Error (NuxmvChecker.MainModuleHasParams pos ) -> Error (MainModuleHasParams pos)
            | Ok _ -> Ok abstract_syntax 
          )
  with 
  | NuxmvLexer.Unexpected_Char c ->
    let pos = Position.get_position lexbuf in Error (UnexpectedChar (pos, c))
  | NuxmvParser.Error ->
    let pos = Position.get_position lexbuf in Error (SyntaxError pos)


let from_channel in_ch =
  parse_buffer (Lexing.from_channel in_ch)

let from_file filename =
  let in_ch = open_in filename in
  let lexbuf = Lexing.from_channel in_ch in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with
                                Lexing.pos_fname = filename };
  parse_buffer lexbuf

let rec print_type_list _type_list = 
  match _type_list with
  | [] -> ""
  | hd :: tail -> (
    let str_hd = print_type hd in
    let str_tail = print_type_list tail in
    if String.length str_tail > 0 
      then str_hd ^ "; " ^ str_tail
      else str_hd
  )

and print_type _type = 
  match _type with
  | NuxmvChecker.IntT -> "Int"
  | NuxmvChecker.SymbolicT -> "Enum Symbol"
  | NuxmvChecker.FloatT -> "Float"
  | NuxmvChecker.EnumT lst-> "Enum of (" ^ print_type_list (List.map snd lst) ^")" 
  | NuxmvChecker.ArrayT lst -> "Array of ("^ print_type_list lst^")"
  | NuxmvChecker.BoolT -> "Bool"
  | NuxmvChecker.SetT lst -> "Set of ("^ print_type_list lst^")"
  | NuxmvChecker.ModuleInstance _ -> "Module"

let of_file filename = 
  match from_file filename with
  | Ok res -> { SubSystem.scope = [] ; source = res ; has_contract = false ; has_modes = false ; has_impl = false ; subsystems = [] }
  | Error error ->(
    match error with
    | UnexpectedChar (pos, char) -> (
      fail_at_position_pt pos 
        ("unexpected character ‘"^ (String.make 1 char) ^ "’") 
      ; raise (Parser_error)
    )
    | SyntaxError (pos) -> (
      fail_at_position_pt pos 
        "syntax error" 
      ; raise (Parser_error)
    )
    | LtlUseError (pos) -> (
      fail_at_position_pt pos 
        ("Invalid use of Ltl Expression")
      ; raise (Parser_error)
    )
    | NextExprError (pos) -> (
      fail_at_position_pt pos 
        ("Invalid use of next expression")
      ; raise (Parser_error)
    )
    | DoubleNextExprError (pos) -> (
      fail_at_position_pt pos 
        ("Cannot use a next expression inside a next expression")
      ; raise (Parser_error)
    )
    | RangeLowerValueError (pos) -> (
      fail_at_position_pt pos 
        ("Lower bound of range must be less than or equal to upper bound")
      ; raise (Parser_error)
    )
    | ExpectedTypeError (pos, _type_list, _type) -> (
      fail_at_position_pt pos 
        ("Given type "^print_type _type^" not in the list of allowed types "^ print_type_list _type_list^" for operation")
      ; raise (Parser_error)
    )
    | NonMatchingTypeError (pos, t1, t2) -> (
      fail_at_position_pt pos 
        ("Type " ^ print_type t1^ " not equal to type " ^ print_type t2)
      ; raise (Parser_error)
    )
    | MissingVariableError (pos, str) -> (
      fail_at_position_pt pos 
        ("Identifier '"^ str ^"' is missing")
      ; raise (Parser_error)
    )
    | VariableAlreadyDefinedError (pos, str) -> (
      fail_at_position_pt pos 
        ("Variable " ^ str ^ " already defined in namespace")
      ; raise (Parser_error)
    )
    | EnumValueExistenceError (pos, str) -> (
      fail_at_position_pt pos 
        ("Enum value "^str^" already exists in enum namespace")
      ; raise (Parser_error)
    )
    | EnumNotContainValue (pos, str) -> (
      fail_at_position_pt pos 
        ("Enum does not contain value "^str)
      ; raise (Parser_error)
    )
    | MainModuleMissing (pos) -> (
      fail_at_position_pt pos 
        ("Main module is missing from given input")
      ; raise (Parser_error)
    )
    | MissingModule (pos, str) -> (
      fail_at_position_pt pos 
        ("Module "^ str ^" is not defined")
      ; raise (Parser_error)
    )
    | ModuleCalledTooManyArgs (pos, i1, i2) -> (
      fail_at_position_pt pos 
        ("Module called with too many arguments, " ^(string_of_int i1)^ " expected and " ^ (string_of_int i2) ^" given")
      ; raise (Parser_error)
    )
    | ModuleCalledMissingArgs (pos, i1, i2) -> (
      fail_at_position_pt pos 
        ("Module called missing arguments, " ^(string_of_int i1)^ " expected and " ^ (string_of_int i2) ^" given")
      ; raise (Parser_error)
    )
    | AccessOperatorAppliedToNonModule (pos) -> (
      fail_at_position_pt pos 
        ("Access operator applied to a non-module instance")
      ; raise (Parser_error)
    )
    | MainModuleHasParams (pos) -> (
      fail_at_position_pt pos 
        ("Main module is not allowed to be defined with parameters")
      ; raise (Parser_error)
    )
    | NotSupportedError(pos, str) -> (
      fail_at_position_pt pos 
        (str ^ " is currently not supported by Kind2 for the Nuxmv language")
      ; raise (Parser_error)
    )
  )
