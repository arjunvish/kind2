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
open Lib

module Ids = Lib.ReservedIds
module A = Analysis
module P = Property

module Ast = NuxmvAst


let filter_map (f : ('a -> 'b option)) (l : 'a list) : 'b list =
    l |> List.map f 
      |> List.filter (fun x -> match x with None -> false | _ -> true)
      |> List.map (fun x -> match x with Some v -> v | _ -> assert false)
    
let find_opt (func : ('a -> bool)) (lst: 'a list) : 'a option =
    try let ans = List.find func lst in Some ans
    with Not_found -> None

let generate_term ref_list svi_map svi_next_map next_expr expr_type = 
    let rec eval_expr next expr = 
        match expr with
        | Ast.True _ -> Term.mk_true ()
        | Ast.False _ -> Term.mk_false ()
        | Ast.CInt (_, i) -> Term.mk_num_of_int i
        | Ast.CFloat (_, f) -> Term.mk_num_of_int (int_of_float f) (* TODO: figure out how to handle reals in internal system *)
        (* Not allowing multiple modules as of now which means we don't need to allow Period Identifiers *)
        | Ast.Ident (_, Ast.CIdent (_, i)) -> (
            let id_res = find_opt (fun x -> match x with (id,t) when i = id -> true| _ -> false) ref_list in 
            match id_res with
            | Some (id, t) -> eval_expr next t  
            | None -> (
                if next 
                then
                    Term.mk_var (filter_map (fun x -> if fst x = i then Some (snd x) else None) svi_next_map |> List.hd)
                else
                    Term.mk_var (filter_map (fun x -> if fst x = i then Some (snd x) else None) svi_map |> List.hd)
            )
        )
        | Ast.CRange (_, i1, i2) -> assert false
        | Ast.Not (_, e) -> Term.mk_not (eval_expr next e)
        | Ast.And (_, e1, e2) -> Term.mk_and ((eval_expr next e1) :: [(eval_expr next e2)])
        | Ast.Or (_, e1, e2) -> Term.mk_or ((eval_expr next e1) :: [(eval_expr next e2)])
        | Ast.Xor (_, e1, e2) -> Term.mk_xor ((eval_expr next e1) :: [(eval_expr next e2)])
        | Ast.Xnor (_, e1, e2) -> Term.mk_not (Term.mk_xor ((eval_expr next e1) :: [(eval_expr next e2)]))
        | Ast.Impl (_, e1, e2) -> Term.mk_implies ((eval_expr next e1) :: [(eval_expr next e2)])
        | Ast.Equiv (_, e1, e2) -> assert false
        | Ast.Eq (_, e1, e2) -> Term.mk_eq ((eval_expr next e1) :: [(eval_expr next e2)])
        | Ast.NotEq (_, e1, e2) -> Term.mk_not (Term.mk_eq ((eval_expr next e1) :: [(eval_expr next e2)]))
        | Ast.Lt (_, e1, e2) -> Term.mk_lt ((eval_expr next e1) :: [(eval_expr next e2)])
        | Ast.Lte (_, e1, e2) -> Term.mk_leq ((eval_expr next e1) :: [(eval_expr next e2)])
        | Ast.Gt (_, e1, e2) -> Term.mk_gt ((eval_expr next e1) :: [(eval_expr next e2)])
        | Ast.Gte (_, e1, e2) -> Term.mk_geq ((eval_expr next e1) :: [(eval_expr next e2)])
        | Ast.Plus (_, e1, e2) -> Term.mk_plus ((eval_expr next e1) :: [(eval_expr next e2)])
        | Ast.Uminus (_, e) -> Term.mk_minus ((Term.mk_num_of_int 0) :: [(eval_expr next e)])
        | Ast.Minus (_, e1, e2) -> Term.mk_minus ((eval_expr next e1) :: [(eval_expr next e2)])
        | Ast.Multiply (_, e1, e2) -> Term.mk_times ((eval_expr next e1) :: [(eval_expr next e2)])
        | Ast.Divide (_, e1, e2) -> Term.mk_div ((eval_expr next e1) :: [(eval_expr next e2)])
        | Ast.Mod (_, e1, e2) -> Term.mk_mod (eval_expr next e1) (eval_expr next e2)
        | Ast.SetExp (_, el) -> assert false
        | Ast.CaseExp (_, e_list) -> (
            let rec create_ite_terms e_list = 
                match e_list with
                | [] -> assert false
                | f :: s :: [] -> Term.mk_ite (eval_expr next (fst f)) (eval_expr next (snd f)) (eval_expr next (fst s))
                | hd :: tail -> Term.mk_ite (eval_expr next (fst hd)) (eval_expr next (snd hd)) (create_ite_terms tail)
            in
            create_ite_terms e_list
        )
        | Ast.IfThenElseExp (_, e1, e2, e3) -> Term.mk_ite (eval_expr next e1) (eval_expr next e2) (eval_expr next e3)
        | Ast.NextExp (_, e) -> eval_expr true e
        | _ -> assert false
    in
    match expr_type with
    | Ast.InvarExpr (_, nuxmv_expr) -> eval_expr next_expr nuxmv_expr
    | Ast.NextExpr (_, nuxmv_expr) -> eval_expr true nuxmv_expr
    | Ast.SimpleExpr (_, nuxmv_expr) -> eval_expr next_expr nuxmv_expr
    | Ast.ArrayExpr (_, nuxmv_expr_list) -> assert false
    | Ast.LtlExpr (_, nuxmv_expr) -> assert false

let create_variables scope main_module = 
    let mod_id, expr_list = 
        match main_module with 
        | Ast.CustomModule (id,_,el) ->(
            let get_svdl = fun x -> match x with Ast.StateVarDecl (_, svdl) -> Some svdl | _ -> None in
            id, (List.hd (filter_map get_svdl el))
        ) 
    in
    let scope' = scope @ [mod_id] in
    let generate_variables state_var_decl =
        match state_var_decl with
        | Ast.ModuleType _ -> None
        | Ast.SimpleType (_, id, type_spec) -> (
            let _type = (
                match type_spec with
                | Ast.Bool _ -> Type.mk_bool ()
                | Ast.Int _ -> Type.mk_int ()
                | Ast.Real _ -> Type.mk_real ()
                | Ast.IntRange (_, i1, i2) -> Type.mk_int_range (Numeral.of_int i1) (Numeral.of_int i2)
                | Ast.EnumType (_, etvl) -> (
                    let val_strings = 
                        List.map 
                            (fun x -> match x with Ast.ETId (_, str) -> str | Ast.ETCInt (_, i) -> string_of_int i)
                            etvl
                    in
                    Type.mk_enum None val_strings
                )
            )
            in
            let state_var = StateVar.mk_state_var id scope' _type in
            Some state_var
        )
    in 
    filter_map generate_variables expr_list

let generate_define_ref_list _module =
    let expr_list = 
        match _module with 
        | Ast.CustomModule (id,_,el) ->(
            let get_del = fun x -> match x with Ast.DefineDecl (_, del) -> Some del | _ -> None in
            (List.hd (filter_map get_del el))
        ) 
    in 
    let rec create_define_process_env del =
        match del with
        | [] -> []
        | de :: tail -> (
            match de with
            | Ast.SimpleDef (pos, id, et) -> (id, et ) :: (create_define_process_env tail)
            | Ast.ArrayDef (pos, id, et) -> (id, et ) :: (create_define_process_env tail)
        )
    in
    create_define_process_env expr_list

let trans_sys_of_nuxmv
    ?(preserve_sig = false)
    ?(slice_nodes = true)
    subsystem analysis_param
    =

    let custom_module_list, env = 
        SubSystem.all_subsystems subsystem
        |> List.map (function { SubSystem.source } -> source) 
        |> List.hd 
        |> function x -> fst x, snd x
    in

    let _module = 
        List.find 
            (fun x -> 
                match x with 
                | Ast.CustomModule (id, [], _) when (String.uppercase_ascii id) = "MAIN" -> true 
                | _ -> false
            )
            custom_module_list
    in
    let state_vars = create_variables [] _module in
    let sv_instance_map, sv_next_instance_map = 
    (List.map (fun x -> (StateVar.name_of_state_var x, Var.mk_state_var_instance x Numeral.zero)) state_vars
    ), (List.map (fun x -> (StateVar.name_of_state_var x, Var.mk_state_var_instance x Numeral.one)) state_vars)
    in

    let ref_list = generate_define_ref_list _module  in

    let new_module_expr_list = 
        filter_map
            (fun x -> (
                match x with
                | Ast.AssignConst (_, acl) -> (
                    let res = 
                        filter_map
                            (fun x -> (
                                match x with
                                | Ast.InitAssign (_, ident, et) -> assert false
                                | Ast.NextAssign (_, ident, et) -> (
                                    match et with
                                    | Ast.SimpleExpr (p, e) -> (
                                        let inclusion_expr = Ast.InclExp (p, Ast.NextExp (p, Ast.Ident (p, Ast.CIdent(p,ident))),e) in
                                        Some (Ast.TransConst (p, Ast.NextExpr (p, inclusion_expr)))
                                    )
                                    | _ -> assert false
                                )
                                | Ast.Assign (_, ident, et) -> (
                                    match et with
                                    | Ast.NextExpr (p, e) -> (
                                        let inclusion_expr = Ast.InclExp (p, Ast.Ident (p,Ast.CIdent(p,ident)), e) in
                                        Some (Ast.InvarConst (p, Ast.NextExpr (p, inclusion_expr)))
                                    ) 
                                    | _ -> assert false
                                    
                                )
                            ))
                            acl
                        in
                        Some res
                )
                | _ -> Some [x]
                )
            )
            (match _module with Ast.CustomModule (_, _, list) -> list)
            |> List.fold_left (fun acc x -> acc @ x) [] 
    in
    let init_expr_set = () in
    ()
