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

module A = NuxmvAst

type semantic_error_type = 
    | LtlUse of Position.t
    | NextExpr of Position.t
    | DoubleNextExpr of Position.t
    | RangeLowerValue of Position.t

type nuxmv_ast_type = 
    | IntT
    | SymbolicT
    | FloatT
    | EnumT of env
    | ArrayT of nuxmv_ast_type list
    | BoolT
    | SetT of nuxmv_ast_type list
    (* | FunT of nuxmv_ast_type list * nuxmv_ast_type *)
    | ModuleInstance of string * env

and env = (string * nuxmv_ast_type) list

type type_error =
    | Expected of Position.t * nuxmv_ast_type list * nuxmv_ast_type
    | NonMatching of Position.t * nuxmv_ast_type * nuxmv_ast_type
    | MissingVariable of Position.t * string
    | VariableAlreadyDefined of Position.t * string
    | EnumValueExist of Position.t * string
    | EnumNotContain of Position.t * string
    | MainError of Position.t (* Main module not defined in program *)
    | MissingModule of Position.t * string
    | ModuleCallTooMany of Position.t * int * int (* Given -> Expected *)
    | ModuleCallMissing of Position.t * int * int (* Given -> Expected *)
    | AccessOperatorAppliedToNonModule of Position.t
    | MainModuleHasParams of Position.t (* Main module has parameters defined *)

type 'a check_result = 
    | CheckOk
    | CheckError of 'a

let find_opt (func : ('a -> bool)) (lst: 'a list) : 'a option =
    try let ans = List.find func lst in Some ans
    with Not_found -> None

let lookup_opt (id : string) (lst : (string * 'a) list ) : (string * 'a) option = 
    find_opt (fun x -> match x with | (s,t) when s = id -> true | _ -> false) lst

let filter_map (f : ('a -> 'b option)) (l : 'a list) : 'b list =
    l |> List.map f 
      |> List.filter (fun x -> match x with None -> false | _ -> true)
      |> List.map (fun x -> match x with Some v -> v | _ -> assert false)

(* SEMANTIC CHECKER *)

let rec s_eval_expr (ltl: bool) (next:bool) (expr: A.nuxmv_expr) : semantic_error_type check_result = 
    match expr with
    | A.True _ -> CheckOk
    | A.False _ -> CheckOk
    | A.CInt _ -> CheckOk
    | A.CFloat _ -> CheckOk
    | A.Ident (_, ci) -> s_eval_complex_id ci
    | A.CRange (p, i1, i2) -> if i1 <= i2 then CheckOk else CheckError (RangeLowerValue (p))
    (* | A.Call (_, ci, nel) -> (
        let result1 = s_eval_complex_id ci in
        match result1 with
        | CheckError _ -> result1
        | CheckOk -> 
            let mapped2 = List.map (s_eval_expr ltl next) nel in
            let result2 = find_opt (fun x -> x != CheckOk) mapped2 in
            match result2 with
            | Some res -> res
            | None -> CheckOk (* How to deal with function call return types *) 
    ) *)
    | A.Not (_, e) -> s_eval_expr ltl next e
    | A.And (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Or (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Xor (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Xnor (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)   
    | A.Impl (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Equiv (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1) 
    | A.Eq (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.NotEq (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Lt (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Lte (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Gt (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Gte (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Plus (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Uminus (_, e) -> s_eval_expr ltl next e
    | A.Minus (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Multiply (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Divide (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Mod (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.SetExp (p, el) ->( 
        if ltl then CheckError (LtlUse (p))
        else
            let mapped = List.map (s_eval_expr ltl false) el in
            let result = find_opt (fun x -> x != CheckOk) mapped in
            match result with
            | Some res -> res
            | None -> CheckOk )
    | A.CaseExp (p, el) -> (
        if ltl then CheckError (LtlUse (p))
        else
            let mapped1 = List.map (s_eval_expr ltl next) (List.map fst el) in
            let result1 = find_opt (fun x -> x != CheckOk) mapped1 in
            match result1 with
            | Some res -> res
            | None -> 
                let mapped2 = List.map (s_eval_expr ltl next) (List.map snd el) in
                let result2 = find_opt (fun x -> x != CheckOk) mapped2 in
                match result2 with
                | Some res -> res
                | None -> CheckOk )
    | A.IfThenElseExp (p, e1, e2, e3) -> (
        match s_eval_expr ltl next e1 with
        | CheckError e -> CheckError e
        | _ ->
            match (s_eval_expr ltl next e2, s_eval_expr ltl next e3) with
            | (CheckOk, CheckOk) -> CheckOk
            | (CheckError e, _) -> CheckError e
            | (_ , CheckError e) -> CheckError e)
    | A.NextExp (p, e) -> (
        if not next then CheckError (NextExpr (p))
        else 
            (match e with 
            | A.NextExp _ -> CheckError (DoubleNextExpr (p))
            | _ -> s_eval_expr ltl next e))
    | A.NextState (p, e) -> 
        if not ltl then CheckError (LtlUse (p))
        else s_eval_expr ltl next e
    | A.Globally (p, e) -> 
        if not ltl then CheckError (LtlUse (p))
        else s_eval_expr ltl next e
    | A.Finally (p, e) -> 
        if not ltl then CheckError (LtlUse (p))
        else s_eval_expr ltl next e
    | A.Until (p, e1, e2) -> (
        if not ltl then CheckError (LtlUse (p))
        else 
            match s_eval_expr ltl next e1 with
            | CheckOk -> s_eval_expr ltl next e2
            | r1 -> r1)
    | A.Releases (p, e1, e2) -> (
        if not ltl then CheckError (LtlUse (p))
        else 
            match s_eval_expr ltl next e1 with
            | CheckOk -> s_eval_expr ltl next e2
            | r1 -> r1)
    | A.PrevState (p, e) -> 
        if not ltl then CheckError (LtlUse (p))
        else s_eval_expr ltl next e
    | A.NotPrevStateNot (p, e) -> 
        if not ltl then CheckError (LtlUse (p))
        else s_eval_expr ltl next e
    | A.Historically (p, e) -> 
        if not ltl then CheckError (LtlUse (p))
        else s_eval_expr ltl next e
    | A.Once (p, e) -> if not ltl then CheckError (LtlUse (p))
                       else s_eval_expr ltl next e
    | A.Since (p, e1, e2) -> (
        if not ltl then CheckError (LtlUse (p))
        else 
            match s_eval_expr ltl next e1 with
            | CheckOk -> s_eval_expr ltl next e2
            | r1 -> r1)
    | A.Triggered (p, e1, e2) -> (
        if not ltl then CheckError (LtlUse (p))
        else 
            match s_eval_expr ltl next e1 with
            | CheckOk -> s_eval_expr ltl next e2
            | r1 -> r1)

and s_eval_expr_type (expr_type: A.expr_type) : semantic_error_type check_result = 
    match expr_type with
    | A.LtlExpr (_, expr) -> s_eval_expr true false expr
    | A.InvarExpr(_, expr) -> s_eval_expr false false expr
    | A.NextExpr (_, expr) -> s_eval_expr false true expr
    | A.SimpleExpr (_, expr) -> s_eval_expr false false expr
    | A.ArrayExpr (_, etl) -> (
        let result_list = List.map s_eval_expr_type etl in
        match find_opt (fun x -> x != CheckOk) result_list with
        | None -> CheckOk
        | Some e -> e
    )

and s_eval_complex_id (ci: A.comp_ident):  semantic_error_type check_result =
    match ci with
    | A.CIdent _ -> CheckOk
    | A.PerIdent _ -> CheckOk

let rec s_eval_state_var_decl (svdl: A.state_var_decl list) :  semantic_error_type check_result =
    match svdl with
    | [] -> CheckOk
    | svd :: t -> 
        match svd with
        | A.ModuleType (p, i, mts) -> (
            match mts with 
            | A.ModuleTypeSpecifier (pos, id, etl) -> (
                let mapped = List.map s_eval_expr_type etl in
                let result = find_opt (fun x -> x != CheckOk) mapped in
                match result with
                | Some res -> res
                | None -> s_eval_state_var_decl t) )
        | _ -> s_eval_state_var_decl t

let rec s_eval_define_decl (del: A.define_element list):  semantic_error_type check_result =
    match del with
    | [] -> CheckOk
    | svd :: t -> 
        match svd with
        | A.SimpleDef (pos, id, et) -> (
            match s_eval_expr_type et with
            | CheckOk -> s_eval_define_decl t
            | other -> other)
        | A.ArrayDef (pos, id, et) -> (
            match s_eval_expr_type et with
            | CheckOk -> s_eval_define_decl t
            | other -> other)

let rec s_eval_assign_const (acl: A.assign_const list):  semantic_error_type check_result =
    match acl with
    | [] -> CheckOk
    | svd :: t -> match svd with
        | A.InitAssign (pos, _, et) -> s_eval_expr_type et
        | A.NextAssign (pos, _, et) -> s_eval_expr_type et
        | A.Assign (pos, _, et) -> s_eval_expr_type et

let s_eval_module_element (me : A.module_element): semantic_error_type check_result = 
    match me with
    | A.StateVarDecl (_, svdl) -> s_eval_state_var_decl svdl
    | A.DefineDecl (_, del) -> s_eval_define_decl del
    | A.AssignConst (_, acl) -> s_eval_assign_const acl
    | A.TransConst (_, expr_type) -> s_eval_expr_type expr_type
    | A.InvarSpec (_, expr_type) -> s_eval_expr_type expr_type
    | A.LtlSpec (_, expr_type) -> s_eval_expr_type expr_type

let rec semantic_eval (ml:A.nuxmv_module list) : semantic_error_type check_result = 
    match ml with
    | [] -> CheckOk
    | A.CustomModule (_, _, mel) :: t -> 
        (let mapped = List.map s_eval_module_element mel in
        let result = find_opt (fun x -> x != CheckOk) mapped in
        match result with
        | Some res -> res
        | None -> semantic_eval t)

(* TYPE CHECKER *)
let enum_contains_value (enum_type_list: (string * nuxmv_ast_type) list ) (id : string) (pos : Position.t) : (bool, type_error) result = 
    let check_list = fun x -> match x with | (s,t) -> if s = id then true else false in
    match List.exists check_list enum_type_list with
    | true -> Ok true
    | false -> Error (EnumNotContain (pos, id))


let rec t_eval_complex_id (in_enum : (bool * env)) (env : env) (ci: A.comp_ident):  (nuxmv_ast_type, type_error) result =
    match ci with
    | A.CIdent (pos, id) -> ( 
        if fst in_enum 
        then
            match enum_contains_value (snd in_enum) id pos with
            | Ok _ -> Ok BoolT
            | Error e -> Error e
        else
            let res = lookup_opt id env in 
            match res with
            | Some (s,t) -> Ok t 
            | None -> Error (MissingVariable (pos, id))
    )
    | A.PerIdent (pos, cid, id) -> (
        let mi = t_eval_complex_id in_enum env cid in
        match mi with 
        | Ok (ModuleInstance (mod_name, mod_env) ) -> (
            match lookup_opt id mod_env with
            | Some (s,t) -> Ok t
            | None -> Error (MissingVariable (pos, id))
        )
        | Ok _ -> Error (AccessOperatorAppliedToNonModule (pos))
        | error -> error
    )

let rec t_eval_expr (in_enum : (bool * env)) (env : env) (expr: A.nuxmv_expr)  : (nuxmv_ast_type, type_error) result = 
    match expr with
    | A.True _ -> Ok BoolT
    | A.False _ -> Ok BoolT
    | A.CInt (pos, i) -> if fst in_enum then t_eval_expr in_enum env (A.Ident (pos, A.CIdent( pos, string_of_int i))) else Ok IntT
    | A.CFloat _ -> Ok FloatT
    | A.Ident (p, ci) -> t_eval_complex_id in_enum env ci
    | A.CRange (p, i1, i2) -> Ok IntT (*TODO: ask Daniel if changing Ranges to Ints make sense for type checking purposes *)
    (* | A.Call (p, ci, nel) -> Ok BoolT (* TODO: figure out how to deal with the call *) *)
    | A.Not (p, e) ->( 
        match t_eval_expr in_enum env e with
        | Ok t when t = BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e)
    | A.And (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e) 
    | A.Or (p, e1, e2) ->  (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e) 
    | A.Xor (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e) 
    | A.Xnor (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e) 
    | A.Impl (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Equiv (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Eq (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok (EnumT enum_list), _ ) -> t_eval_expr (true,enum_list) env e2 
        | ( _ , Ok (EnumT enum_list) ) -> t_eval_expr (true,enum_list) env e1
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok BoolT
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [IntT; BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.NotEq (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok (EnumT enum_list), _ ) -> t_eval_expr (true,enum_list) env e2 
        | ( _ , Ok (EnumT enum_list) ) -> t_eval_expr (true,enum_list) env e1
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok BoolT
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [IntT; BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Lt (p, e1, e2) ->  ( 
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok BoolT
            | _ -> Error (Expected (p, [IntT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Lte (p, e1, e2) -> ( 
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok BoolT
            | _ -> Error (Expected (p, [IntT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Gt (p, e1, e2) ->  ( 
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok BoolT
            | _ -> Error (Expected (p, [IntT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Gte (p, e1, e2) -> ( 
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok BoolT
            | _ -> Error (Expected (p, [IntT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Plus (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok IntT
            | FloatT -> Ok FloatT
            | _ -> Error (Expected (p, [IntT; FloatT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Uminus (p, e) -> (
        match t_eval_expr in_enum env e with
        | Ok t when t = IntT -> Ok IntT
        | Ok t -> Error (Expected (p, [IntT], t))
        | err -> err)
    | A.Minus (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok IntT
            | FloatT -> Ok FloatT
            | _ -> Error (Expected (p, [IntT; FloatT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Multiply (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok IntT
            | FloatT -> Ok FloatT
            | _ -> Error (Expected (p, [IntT; FloatT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Divide (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok IntT
            | FloatT -> Ok FloatT
            | _ -> Error (Expected (p, [IntT; FloatT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Mod (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok IntT
            | _ -> Error (Expected (p, [IntT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.SetExp (p, el) ->  (
        let mapped = List.map (t_eval_expr in_enum env) el in
        let result = find_opt (fun x -> match x with | Ok _ -> false | _ -> true) mapped in
        match result with
        | Some e -> e
        | None -> Ok (SetT (List.map (fun x -> match x with Ok t -> t | _ -> assert false) mapped)))
    | A.CaseExp (p, el) -> (
        let left_expr_res = List.map (t_eval_expr (false, []) env) (List.map fst el) in
        match find_opt (fun x -> match x with Ok BoolT -> false | _ -> true) left_expr_res with
        | Some (Error e) -> Error e
        | Some (Ok t) -> Error (Expected (p, [BoolT], t))
        | None -> (
            let right_expr_res = List.map (t_eval_expr in_enum env) (List.map snd el) in 
            match find_opt (fun x -> match x with Ok _ -> false | _ -> true) right_expr_res with
            | Some (Error e) -> Error e
            | None -> (
                match find_opt (fun x -> match x with Ok SetT _ -> true | _ -> false) right_expr_res with
                | Some _ -> (
                    let turn_into_sets lst = List.map (fun x -> match x with Ok (SetT x') -> SetT x' | Ok x'-> SetT [x'] | _ -> assert false) lst in
                    Ok (SetT (turn_into_sets right_expr_res))
                )
                | None -> (
                    let head_type = match (List.hd right_expr_res) with Ok _type -> _type | _ -> assert false in
                    let check_type_function = 
                        (fun x -> match x with | Ok x when x = head_type -> None | Ok x -> Some x | _ -> assert false ) 
                    in
                    let different_return_types = filter_map check_type_function (List.tl right_expr_res) in
                    if List.length different_return_types > 0 
                        then Error (NonMatching (p, head_type, List.hd (List.tl different_return_types)))
                        else Ok head_type
                )
            )
            | _ -> assert false
        )
    )
    | A.IfThenElseExp (p, e1, e2, e3) -> Ok BoolT (* TODO: Finish this up when have time, added as an extra but no test fragments use it*) 
    | A.NextExp (p, e) -> (
        match t_eval_expr in_enum env e with
        | Ok t -> Ok t
        | e -> e
    )
    | A.NextState (p, e) ->  (
        match t_eval_expr in_enum env e with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e
    )
    | A.Globally (p, e) ->  (
        match t_eval_expr in_enum env e with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e
    )
    | A.Finally (p, e) -> (
        match t_eval_expr in_enum env e with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e
    )
    | A.Until (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e
    )
    | A.Releases (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e
    )
    | A.PrevState (p, e) ->  (
        match t_eval_expr in_enum env e with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e
    )
    | A.NotPrevStateNot (p, e) ->  (
        match t_eval_expr in_enum env e with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e
    )
    | A.Historically (p, e) ->  (
        match t_eval_expr in_enum env e with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e
    )
    | A.Once (p, e) -> (
        match t_eval_expr in_enum env e with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e
    )
    | A.Since (p, e1, e2) ->  (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e
    )
    | A.Triggered (p, e1, e2) -> (
        match (t_eval_expr in_enum env e1, t_eval_expr in_enum env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e
    )

let rec t_eval_expr_type (in_enum: (bool * env)) (env : env) (expr_type: A.expr_type) : (nuxmv_ast_type, type_error) result  = 
    match expr_type with
    | A.LtlExpr (p, expr) -> (
        match t_eval_expr in_enum env expr with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e
    )
    | A.InvarExpr (p, expr) -> (
        match t_eval_expr in_enum env expr with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e
    )
    | A.NextExpr (_, expr) -> (
        match t_eval_expr in_enum env expr with
        | Ok t -> Ok t
        | e -> e
    )
    | A.SimpleExpr (_, expr) ->(
        match t_eval_expr in_enum env expr with
        | Ok t -> Ok t
        | e -> e
    )
    | A.ArrayExpr (_, etl) -> (
        let result_list = List.map (t_eval_expr_type in_enum env) etl in
        match find_opt (fun x -> match x with | Ok _ -> false | Error _ -> true) result_list with
        | Some e -> e
        | None -> Ok (ArrayT (List.map (fun x -> match x with Ok t -> t | _ -> assert false) result_list))
    )

let rec t_eval_enum_var_decl (etvl: A.enum_type_value list) : (((string * nuxmv_ast_type) list, type_error) result)=
    let exist = fun y x -> match x with (s,t) -> if s = y then true else false in
    match etvl with
    | [] -> Ok []
    (* Do not add the enum values to the overall namespace *)
    | etv :: tail ->(
        match etv with 
        | A.ETId (pos, id) -> (
            let res = t_eval_enum_var_decl tail in
            match res with
             Ok lst -> 
                if List.exists (exist id) lst 
                then Error (EnumValueExist (pos, id))
                else Ok ((id, SymbolicT) :: lst)
            | error -> error)
        | A.ETCInt (pos, v) -> (
            let res = t_eval_enum_var_decl tail in
            match res with
             Ok lst -> 
                if List.exists (exist (string_of_int v) ) lst 
                then Error (EnumValueExist (pos, string_of_int v ))
                else Ok ((string_of_int v, IntT) :: lst)
            | error -> error))

let rec set_param_values (param_ids : string list) (natl : nuxmv_ast_type list) (finished_env: env) : env =
    match (param_ids, natl) with 
    | ( [] , []) -> finished_env
    | ( id :: r_ids, nat :: r_nats) -> (
        let new_env = (id, nat) :: finished_env in
        set_param_values r_ids r_nats new_env
    )
    | _ -> assert false

let rec t_eval_state_var_decl (env : env) (svdl: A.state_var_decl list) (mod_def : A.nuxmv_module list) : (env, type_error) result =
    match svdl with
    | [] -> Ok env 
    | svd :: t  -> 
        (match svd with 
        | A.ModuleType (p, i, mts) -> 
            (match mts with 
            | A.ModuleTypeSpecifier (pos, id, etl) -> 
            let mapped = List.map (t_eval_expr_type (false, []) env) etl in
            let result = find_opt (fun x -> match x with Ok _ -> false | Error _ -> true) mapped in
            match result with
            | Some Error e -> Error e
            | None -> (
                let find_mod = fun x -> match x with A.CustomModule (m_id, _, _) when id = m_id -> true | _ -> false in
                match find_opt find_mod mod_def with
                | Some (A.CustomModule (mod_id, param_ids, mel)) -> (
                    let values = List.map (fun x -> match x with Ok v -> v | _ -> assert false) mapped in
                    let num_exp = List.length values in
                    let num_id = List.length param_ids in
                    if num_id > num_exp then Error (ModuleCallMissing (pos, num_exp, num_id))
                    else if num_id < num_exp then Error (ModuleCallTooMany (pos, num_exp, num_id))
                    else (
                        let mod_param_env = set_param_values param_ids values [] in
                        match type_eval_module_element_list mod_param_env mel mod_def with
                        | Ok mod_env -> (
                            let mod_i = ModuleInstance  (id, mod_env) in
                            let env' = (i, mod_i) :: env in 
                            t_eval_state_var_decl env' t mod_def
                        )
                        | Error e -> Error e
                    )
                )
                | None -> Error (MissingModule (pos, id))
            )
            | _ -> assert false
        )
        | A.SimpleType (p, i, sts) -> 
            (match sts with
                (* TODO: Check that the identifier isn't being used already *)
                | A.Bool _ -> (
                    match lookup_opt i env with
                    | Some _ -> Error (VariableAlreadyDefined (p, i))
                    | None -> let env' = (i, BoolT) :: env in t_eval_state_var_decl env' t mod_def
                )   
                | A.Int _ -> (
                    match lookup_opt i env with
                    | Some _ -> Error (VariableAlreadyDefined (p, i))
                    | None -> let env' = (i, IntT) :: env in t_eval_state_var_decl env' t mod_def
                ) 
                | A.Real _ -> (
                    match lookup_opt i env with
                    | Some _ -> Error (VariableAlreadyDefined (p, i))
                    | None -> let env' = (i, FloatT) :: env in t_eval_state_var_decl env' t mod_def
                ) 
                | A.IntRange _ -> (
                    match lookup_opt i env with
                    | Some _ -> Error (VariableAlreadyDefined (p, i))
                    | None -> let env' = (i, IntT) :: env in t_eval_state_var_decl env' t mod_def
                ) 
                | A.EnumType (p, etvl) -> ( 
                    let enumLst = t_eval_enum_var_decl etvl in
                    match enumLst with
                    | Ok lst -> let env' = (i, EnumT lst) :: env in t_eval_state_var_decl env' t mod_def
                    | Error e -> Error e)
            )
        )

and create_define_process_env (del: A.define_element list): (string * A.expr_type) list = 
    match del with
    | [] -> []
    | svd :: tail ->
        match svd with
        | A.SimpleDef (pos, id, et) -> (id, et ) :: (create_define_process_env tail)
        | A.ArrayDef (pos, id, et) -> (id, et ) :: (create_define_process_env tail)

and t_process_define_variables (env : env) (unprocessed_env: (string * A.expr_type) list) : (env, type_error) result =
    match unprocessed_env with
    | [] -> Ok env
    | (id, expr_type ) :: tail -> (
        match lookup_opt id env with
        | None -> (
            match t_eval_expr_type (false, []) env expr_type with
            | Ok  t -> (
                let newEnv = (id, t) :: env in
                t_process_define_variables newEnv tail
            )
            | Error (MissingVariable (p, mvid)) -> (
                let contains = lookup_opt mvid tail in
                match contains with
                | Some (mvid, expr_type') -> (
                    let filtered_tail = List.filter (fun x -> match x with | (s,t) when s = mvid -> false | _ -> true) tail in
                    let new_unprocessed_order = (mvid, expr_type') :: (id, expr_type) :: filtered_tail in
                    t_process_define_variables env new_unprocessed_order
                )
                | None -> Error (MissingVariable (p, mvid))
            )
            | Error e -> Error e
        )
        | Some _ -> (
            match expr_type with
            | A.LtlExpr (p, _) -> Error (VariableAlreadyDefined (p, id))
            | A.InvarExpr (p, _) -> Error (VariableAlreadyDefined (p, id))
            | A.NextExpr (p, _) -> Error (VariableAlreadyDefined (p, id))
            | A.SimpleExpr (p, _) ->Error (VariableAlreadyDefined (p, id))
            | A.ArrayExpr (p, _) -> Error (VariableAlreadyDefined (p, id))
        )
    )

and t_eval_assign_const (env : env) (acl: A.assign_const list): (env, type_error) result =
    match acl with
    | [] -> Ok env
    | svd :: tail -> (
        match svd with
        | A.InitAssign (pos, id, et) -> (
            match lookup_opt id env with
            | Some (_, EnumT lst) -> (
                match t_eval_expr_type (true, (id, (EnumT lst)) :: lst) env et with
                | Ok _ -> t_eval_assign_const env tail
                | Error e -> Error e
            )
            | Some (_,t) -> (
                match t_eval_expr_type (false, []) env et with
                | Ok t' when t' = t -> t_eval_assign_const env tail 
                | Ok t' -> Error (NonMatching (pos, t', t))
                | Error e -> Error e
            )
            | None -> Error (MissingVariable (pos, id))
        )
        | A.NextAssign (pos, id, et) -> (
            match lookup_opt id env with
            | Some (_, EnumT lst) -> (
                match t_eval_expr_type (true, (id, (EnumT lst)) :: lst) env et with
                | Ok _ -> t_eval_assign_const env tail 
                | Error e -> Error e
            )
            | Some (_,t) -> (
                match t_eval_expr_type (false, []) env et with
                | Ok t' when t' = t -> t_eval_assign_const env tail 
                | Ok t' -> Error (NonMatching (pos, t', t))
                | Error e -> Error e
            )
            | None -> Error (MissingVariable (pos, id))
        )
        | A.Assign (pos, id, et) -> (
            match lookup_opt id env with
            | Some (_, EnumT lst) -> (
                match t_eval_expr_type (true, lst) env et with
                | Ok _ -> t_eval_assign_const env tail 
                | Error e -> Error e
            )
            | Some (_,t) -> (
                match t_eval_expr_type (false, []) env et with
                | Ok t' when t' = t -> t_eval_assign_const env tail 
                | Ok t' -> Error (NonMatching (pos, t', t))
                | Error e -> Error e
            )
            | None -> Error (MissingVariable (pos, id))
        )
    )

and t_eval_module_element (env : env) (me : A.module_element) (mod_def : A.nuxmv_module list): (env, type_error) result = 
    match me with
    | A.StateVarDecl (_, svdl) -> t_eval_state_var_decl env svdl mod_def
    | A.DefineDecl (_, del) -> (
        let unprocessed_env = create_define_process_env del in 
        t_process_define_variables env unprocessed_env
    )
    | A.AssignConst (_, acl) -> t_eval_assign_const env acl
    | A.TransConst (_, expr_type) -> (
        match t_eval_expr_type (false, []) env expr_type with
        | Ok _ -> Ok env
        | Error e -> Error e
    )
    | A.InvarSpec (_, expr_type) -> (
        match t_eval_expr_type (false, []) env expr_type with
        | Ok _ -> Ok env
        | Error e -> Error e
    )
    | A.LtlSpec (_, expr_type) -> (
        match t_eval_expr_type (false, []) env expr_type with
        | Ok _ -> Ok env
        | Error e -> Error e
    )

and type_eval_module_element_list (env : env) (mel: A.module_element list) (mod_def: A.nuxmv_module list) : (env, type_error) result = 
    match mel with
    | [] -> Ok env
    | me :: t -> 
        match t_eval_module_element env me mod_def with
        | Ok env' -> type_eval_module_element_list env' t mod_def
        | error -> error

and type_eval_rec (mod_def : A.nuxmv_module list) (ml:A.nuxmv_module list) : (env, type_error) result = 
    match ml with
    | [] -> (
        let get_main = fun x -> match x with A.CustomModule (id, [], _) when (String.uppercase_ascii id) = "MAIN" -> true | _ -> false in
        match find_opt get_main mod_def with
        | Some A.CustomModule (id, [], mel) -> type_eval_module_element_list [] mel mod_def
        | Some _ -> Error (MainModuleHasParams Position.create_empty_position)
        | None -> Error (MainError Position.create_empty_position)
    )
    | c_mod :: t -> 
        type_eval_rec (c_mod :: mod_def) t 

let type_eval (ml : A.nuxmv_module list) : (env, type_error) result = type_eval_rec [] ml