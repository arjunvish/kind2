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

let generate_term ref_list svi_map svi_next_map env next_expr expr_type = 
    let rec eval_expr next enum in_set expr = 
        match expr with
        | Ast.True _ -> Term.mk_true ()
        | Ast.False _ -> Term.mk_false ()
        | Ast.CInt (_, i) -> Term.mk_num i
        | Ast.CFloat (_, f) -> Term.mk_dec f
        (* Not allowing multiple modules as of now which means we don't need to allow Period Identifiers *)
        | Ast.Ident (_, Ast.CIdent (_, i)) -> (
            let id_res = find_opt (fun x -> match x with (id,t) when i = id -> true| _ -> false) ref_list in 
            match id_res with
            | Some (id, t) -> eval_expr next enum in_set t  
            | None -> (
                match enum, in_set, next with
                | (true, false, _) -> Term.mk_true () (* TODO: Determine how to make a lustreNode contract here for the single value  *)
                | (true, true, _) -> Term.mk_true () (* TODO: Find way to return jsut the string of the enum value back for the set contract *)
                | (false, _, true) -> Term.mk_var (filter_map (fun x -> if fst x = i then Some (snd x) else None) svi_next_map |> List.hd)
                | (false, _, false) -> Term.mk_var (filter_map (fun x -> if fst x = i then Some (snd x) else None) svi_map |> List.hd)
            )
        )
        | Ast.CRange (_, i1, i2) -> assert false
        | Ast.Not (_, e) -> Term.mk_not (eval_expr next enum in_set e)
        | Ast.And (_, e1, e2) -> Term.mk_and ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)])
        | Ast.Or (_, e1, e2) -> Term.mk_or ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)])
        | Ast.Xor (_, e1, e2) -> Term.mk_xor ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)])
        | Ast.Xnor (_, e1, e2) -> Term.mk_not (Term.mk_xor ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)]))
        | Ast.Impl (_, e1, e2) -> Term.mk_implies ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)])
        | Ast.Equiv (_, e1, e2) -> assert false
        | Ast.Eq (_, e1, e2) -> Term.mk_eq ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)])
        | Ast.NotEq (_, e1, e2) -> Term.mk_not (Term.mk_eq ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)]))
        | Ast.Lt (_, e1, e2) -> Term.mk_lt ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)])
        | Ast.Lte (_, e1, e2) -> Term.mk_leq ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)])
        | Ast.Gt (_, e1, e2) -> Term.mk_gt ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)])
        | Ast.Gte (_, e1, e2) -> Term.mk_geq ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)])
        | Ast.Plus (_, e1, e2) -> Term.mk_plus ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)])
        | Ast.Uminus (_, e) -> Term.mk_minus ((Term.mk_num_of_int 0) :: [(eval_expr next enum in_set e)])
        | Ast.Minus (_, e1, e2) -> Term.mk_minus ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)])
        | Ast.Multiply (_, e1, e2) -> Term.mk_times ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)])
        | Ast.Divide (_, e1, e2) -> Term.mk_div ((eval_expr next enum in_set e1) :: [(eval_expr next enum in_set e2)])
        | Ast.Mod (_, e1, e2) -> Term.mk_mod (eval_expr next enum in_set e1) (eval_expr next enum in_set e2)
        | Ast.CaseExp (_, e_list) -> (
            let rec create_ite_terms e_list = 
                match e_list with
                | [] -> assert false
                | f :: s :: [] -> Term.mk_ite (eval_expr next enum in_set (fst f)) (eval_expr next enum in_set (snd f)) (eval_expr next enum in_set (fst s))
                | hd :: tail -> Term.mk_ite (eval_expr next enum in_set (fst hd)) (eval_expr next enum in_set (snd hd)) (create_ite_terms tail)
            in
            create_ite_terms e_list
        )
        | Ast.IfThenElseExp (_, e1, e2, e3) -> Term.mk_ite (eval_expr next enum in_set e1) (eval_expr next enum in_set e2) (eval_expr next enum in_set e3)
        | Ast.NextExp (_, e) -> eval_expr true enum in_set e
        (* TODO: Determine what to do with incl operator (only generated in trans sys creation) *)
        | Ast.InclExp (_, e1, e2) -> (
            let term_e1 = eval_expr next enum in_set e1 in
            if Type.is_enum (Term.type_of_term term_e1)
            then eval_expr next true in_set e2
            else (
                let term_e2 = eval_expr next enum in_set e2 in
                Term.mk_eq [term_e1; term_e2]
            )
        )
        (* TODO: Determine how to do the node creation with the contract check for being one of all possible expressions *)
        | Ast.SetExp (_, el) -> (
            let el_map = List.map (fun x -> eval_expr next enum true x) el in
            (* Sets are only used for enums therefore we are going to take all contracts made from each individual one and create a sum contract *)
            Term.mk_true ()
        )
        | _ -> assert false
    in
    match expr_type with
    | Ast.InvarExpr (_, nuxmv_expr) -> eval_expr next_expr false false nuxmv_expr
    | Ast.NextExpr (_, nuxmv_expr) -> eval_expr next_expr false false nuxmv_expr
    | Ast.SimpleExpr (_, nuxmv_expr) -> eval_expr next_expr false false nuxmv_expr
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
                | Ast.IntRange (_, i1, i2) -> Type.mk_int_range i1 i2
                | Ast.EnumType (_, etvl) -> (
                    let val_strings = 
                        List.map 
                            (fun x -> match x with Ast.ETId (_, str) -> str | Ast.ETCInt (_, i) -> Numeral.string_of_numeral i)
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
            | Ast.SimpleDef (pos, id, et) -> (
                let expr = (
                    match et with 
                    | Ast.SimpleExpr (_, e) -> e
                    | Ast.NextExpr (_, e) -> e
                    | Ast.InvarExpr (_, e) -> e
                    | _ -> assert false
                ) in (id, expr ) :: (create_define_process_env tail)
            )
            | Ast.ArrayDef (pos, id, et) -> (
                let expr = (
                    match et with 
                    | Ast.SimpleExpr (_, e) -> e
                    | Ast.NextExpr (_, e) -> e
                    | Ast.InvarExpr (_, e) -> e
                    | _ -> assert false
                ) in (id, expr ) :: (create_define_process_env tail)
            )
        )
    in
    create_define_process_env expr_list

let generate_all_terms new_mod_expr_list ref_list svi_map svni_map env init_flag= 
        let init_terms = 
            let filtered = (
                filter_map
                    (fun x -> 
                        match x with 
                        | Ast.InitConst (_, expr_type) -> (
                            let term = 
                                generate_term ref_list svi_map svni_map env false expr_type 
                            in
                            Some term
                        )
                        | _ -> None
                    ) 
                    new_mod_expr_list 
            ) in
            match filtered with
            | [] -> assert false
            | hd :: tail -> (
                let new_hd = 
                    Term.mk_let 
                        [(Var.mk_state_var_instance init_flag TransSys.init_base, Term.mk_true ())] 
                        hd 
                in 
                new_hd :: tail
            )
        in

        let trans_terms = 
            let filtered = (
                filter_map
                    (fun x -> 
                        match x with 
                        | Ast.TransConst (_, expr_type) -> (
                            let term = 
                                generate_term ref_list svi_map svni_map env true expr_type 
                            in
                            Some term
                        )
                        | _ -> None
                    ) 
                    new_mod_expr_list 
            ) in
            match filtered with
            | [] -> assert false
            | hd :: tail -> (
                let new_hd = 
                    Term.mk_let 
                        [(Var.mk_state_var_instance init_flag TransSys.trans_base, Term.mk_true ())] 
                        hd 
                in 
                new_hd :: tail
            )
        in 
        let prop_num = 0 in
        let properties = 
            let prop_status = P.PropUnknown in
            filter_map
                (fun x -> 
                    match x with 
                    | Ast.InvarConst (pos, expr_type) -> (
                        let prop_term = 
                            generate_term ref_list svi_map svni_map env true expr_type 
                        in
                        let lib_pos = pos_of_file_row_col (pos.Position.fname, pos.Position.line, pos.Position.col) in
                        let prop_source = P.PropAnnot (lib_pos) in
                        let prop_name = string_of_int prop_num in
                        let return_prop = 
                        {   P.prop_name; 
                            P.prop_source; 
                            P.prop_term; 
                            P.prop_status;  } 
                        in
                        prop_num <= prop_num + 1 |> ignore ; Some (return_prop)
                    )
                    | _ -> None
                ) 
                new_mod_expr_list 
        in

        init_terms, trans_terms, properties

let trans_sys_of_nuxmv
    ?(preserve_sig = false)
    ?(slice_nodes = true)
    subsystem analysis_param
    =

    let custom_module_list, env = 
        SubSystem.all_subsystems subsystem
        |> List.map (function { SubSystem.source } -> source) 
        |> List.hd
        |> (fun x -> fst x, snd x)
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

    let scope = ["Main"] in

    let init_flag = StateVar.mk_init_flag ["main_init_flag"] in

    let state_vars = create_variables [] _module in

    let sv_instance_map, sv_next_instance_map = 
        (List.map (fun x -> (StateVar.name_of_state_var x, Var.mk_state_var_instance x Numeral.zero)) state_vars)
        , (List.map (fun x -> (StateVar.name_of_state_var x, Var.mk_state_var_instance x Numeral.one)) state_vars)
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
                                | Ast.InitAssign (_, ident, et) -> (
                                    match et with
                                    | Ast.SimpleExpr (p, e) -> (
                                        let inclusion_expr = Ast.InclExp (p, Ast.NextExp (p, Ast.Ident (p, Ast.CIdent(p,ident))),e) in
                                        Some (Ast.InitConst (p, Ast.SimpleExpr (p, inclusion_expr)))
                                    )
                                    | _ -> assert false
                                )
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

    let init_terms, trans_terms, properties = 
        generate_all_terms 
            new_module_expr_list 
            ref_list
            sv_instance_map
            sv_next_instance_map
            env
            init_flag
    in

    let node_assumptions =
        (* No assumptions if abstract. *)
        if A.param_scope_is_abstract analysis_param scope then
            Invs.empty ()
        else
            A.param_assumptions_of_scope analysis_param scope
    in

    let valid_prop_terms =
        List.fold_left
            (fun acc ({ P.prop_term } as p) ->
                match Invs.find node_assumptions prop_term with
                | None -> acc
                | Some cert -> (
                    P.set_prop_invariant p cert; (* Set property valid *)
                    prop_term :: acc
                )
            )
            [] 
            properties
    in

    let init_terms, trans_terms =
        (* Iterate over each valid property term *)
        List.fold_left
        (fun
            (init_terms, trans_terms) prop_term ->

            (* Bump term to offset of initial state constraint *)
            let prop_term_init =
                Term.bump_state
                Numeral.(TransSys.init_base - TransSys.prop_base)
                (Term.mk_implies [prop_term])
            in

            (* Bump term to offset of transition relation *)
            let prop_term_trans =
                Term.bump_state
                Numeral.(TransSys.trans_base - TransSys.prop_base)
                (Term.mk_implies [prop_term])
            in

                (* Add property as assertion *)
                (prop_term_init :: init_terms,
                prop_term_trans :: trans_terms)
            )

            (init_terms, trans_terms)

        valid_prop_terms
    in

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

    (* Create instances of state variables in signature *)
    let trans_formals = 
        (* All state variables at the current instant. *)
        List.map 
            (fun sv ->
                Var.mk_state_var_instance sv TransSys.trans_base)
            state_vars 
        @
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
                Ids.trans_uf_string
                "vmt_program"
                (A.info_of_param analysis_param).A.uid)
            (List.map Var.type_of_var trans_formals)
            Type.t_bool
    in

    let trans_sys, _ = 
        TransSys.mk_trans_sys 
            scope
            None (* No instance variables *)
            init_flag
            [] (* global_state_vars *)
            state_vars
            (StateVar.StateVarHashtbl.create 0) (* state_var_bounds *)
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
            (None, []) (* mode_requires *)
            (Invs.empty ()) (* invariants *)
    in
    trans_sys, subsystem

    
