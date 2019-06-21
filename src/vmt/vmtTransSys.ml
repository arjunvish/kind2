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

module Ast = VmtAst


let filter_map (f : ('a -> 'b option)) (l : 'a list) : 'b list =
    l |> List.map f 
      |> List.filter (fun x -> match x with None -> false | _ -> true)
      |> List.map (fun x -> match x with Some v -> v | _ -> assert false)
    
let find_opt (func : ('a -> bool)) (lst: 'a list) : 'a option =
    try let ans = List.find func lst in Some ans
    with Not_found -> None

let rec find_spec_exprs (expr_list : VmtAst.t) = 
    let rec generate_full_expr (ref_list: (string * Ast.sort * Ast.term) list) (term: Ast.term) =
        match term with
        | Ast.Ident (pos, i) -> (
            let id_res = find_opt (fun x -> match x with (id,rt,t) when i = id -> true| _ -> false) ref_list in 
            match id_res with
            | Some (id, _, t) -> generate_full_expr ref_list term
            | None -> Ast.Ident(pos, i) |> generate_full_expr ref_list
        )
        | Ast.Operation (pos, op, tl) -> (
            let tl' = List.map (generate_full_expr ref_list) tl in
            Ast.Operation (pos, op, tl')
        )
        | Ast.AttributeTerm (pos, term, att) -> Ast.AttributeTerm (pos, generate_full_expr ref_list term, att)
        | _ -> term
    in
    let ref_list = 
        filter_map 
        (fun x -> match x with Ast.DefineFun (_, id, [], rt, term) -> Some (id, rt, term) | _ -> None) 
        expr_list 
    in
    
    let init_expr =
        let init_check x = 
            match x with 
            | (id, rt, Ast.AttributeTerm (pos, term', Ast.InitTrue _)) -> (
                Some (id, rt, generate_full_expr ref_list term')
            )
            | _ -> None
        in
        filter_map init_check ref_list |> List.hd
    in

    let trans_expr =
        let trans_check x = 
            match x with 
            | (id, rt, Ast.AttributeTerm (pos, term', Ast.TransTrue _)) -> (
                Some (id, rt, generate_full_expr ref_list term')
            )
            | _ -> None
        in
        filter_map trans_check ref_list |> List.hd
    in

    let prop_exprs = 
            let prop_check x = 
            match x with 
            | (id, rt, Ast.AttributeTerm (pos, term', Ast.LiveProperty (p, num))) -> (
                Some (id, rt, Ast.AttributeTerm (pos, generate_full_expr ref_list term', Ast.LiveProperty (p, num)))
            )
            | (id, rt, Ast.AttributeTerm (pos, term', Ast.InvarProperty (p, num) )) -> (
                Some (id, rt, Ast.AttributeTerm (pos, generate_full_expr ref_list term', Ast.InvarProperty (p, num)))
            )
            | _ -> None
        in
        filter_map prop_check ref_list
    in
    init_expr, trans_expr, prop_exprs

let determine_var (scope : Scope.t) (expr : Ast.vmt_expr): StateVar.t option =
    match expr with
    | Ast.DeclareFun (pos, ident, [], sort) ->
    (
        let _type = Type.mk_bool () in (* Default making all variables a bool will specify types later in ast*)
        let state_var = StateVar.mk_state_var ident scope _type in
        Some state_var
    )
    | _ -> None

let trans_sys_of_vmt 
    ?(preserve_sig = false)
    ?(slice_nodes = true)
    subsystem analysis_param
    : unit (* TransSys.t * VmtAst.t SubSystem.t *) =

    let expr_list = 
        SubSystem.all_subsystems subsystem
        |> List.map (function { SubSystem.source } -> source) 
        |> List.hd
    in

    let init_expr, trans_expr, prop_exprs = find_spec_exprs expr_list in

    let scope = ["TEST"] in (* Wait to determine how to get the name for the scope *)

    let state_vars = expr_list |> filter_map (determine_var scope) in

    let init_flag = () in

    (* Only one set of variables in the Vmt program and all are treated as output *)
    let init_formals = 
        List.map
            (fun sv -> 
                Var.mk_state_var_instance sv TransSys.init_base)
            state_vars
    in

    let init_uf_symbol = 
        UfSymbol.mk_uf_symbol
            (Format.asprintf
                "%s_%s_%d"
                Ids.init_uf_string
                "Vmt_Program"
                (A.info_of_param analysis_param).A.uid)
            (List.map Var.type_of_var init_formals)
            Type.t_bool
    in

    let init_terms = () in

    (* Create instances of state variables in signature *)
    let trans_formals = 
        (* All state variables at the current instant. *)
        List.map 
            (fun sv ->
                Var.mk_state_var_instance sv TransSys.trans_base)
            state_vars @

        (* Non-constant state variables at the previous instant *)
        List.map 
            (fun sv -> 
                Var.mk_state_var_instance 
                sv
                (TransSys.trans_base |> Numeral.pred))
            (List.filter
                (fun sv -> not (StateVar.is_const sv)) 
                state_vars)
    in 

    let trans_uf_symbol = 
        UfSymbol.mk_uf_symbol
            (Format.asprintf
                "%s_%s_%d"
                Ids.init_uf_string
                "Vmt_Program"
                (A.info_of_param analysis_param).A.uid)
            (List.map Var.type_of_var trans_formals)
            Type.t_bool
    in

    let trans_terms = () in

    let properties = [] in

    (* let trans_sys = 
        TransSys.mk_trans_sys 
            scope
            None (* No instance variables *)
            init_flag
            [] (* global_state_vars *)
            state_vars
            [] (* state_var_bounds *)
            [] (* Global Const *)
            [] (* UFS *)
            init_uf_symbol
            init_formals
            (Term.mk_and init_terms)
            trans_uf_symbol
            trans_formals
            (Term.mk_and trans_terms)
            [] (* Subsystems *)
            properties
            [] (* mode_requires *)
            Invs.empty (* invariants *)
    in
    trans_sys *)
    ()
    
