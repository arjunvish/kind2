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

module Ast = VmtAst


let filter_map (f : ('a -> 'b option)) (l : 'a list) : 'b list =
    l |> List.map f 
      |> List.filter (fun x -> match x with None -> false | _ -> true)
      |> List.map (fun x -> match x with Some v -> v | _ -> assert false)
    
let find_opt (func : ('a -> bool)) (lst: 'a list) : 'a option =
    try let ans = List.find func lst in Some ans
    with Not_found -> None

let rec generate_full_expr ref_list svi_map term =
    match term with
    | Ast.Ident (pos, i) -> (
        let id_res = find_opt (fun x -> match x with (id,rt,t) when i = id -> true| _ -> false) ref_list in 
        match id_res with
        | Some (id, _, t) -> generate_full_expr ref_list svi_map t  
        | None -> Term.mk_var (filter_map (fun x -> if fst x = i then Some (snd x) else None) svi_map |> List.hd)
    )
    | Ast.Operation (pos, op, tl) -> (
        let tl' = List.map (generate_full_expr ref_list svi_map) tl in
        (* TODO: make this operation conversion exhasutive to all allowed terms in term.mli *)
        match op with
        | "not" -> Term.mk_not (List.hd tl')
        | "or" -> Term.mk_or tl'
        | "and" -> Term.mk_and tl'
        | "xor" -> Term.mk_xor tl'
        | "to_real" -> Term.mk_to_real (List.hd tl')
        | "to_int" -> Term.mk_to_int (List.hd tl')
        | "is_int" -> Term.mk_is_int (List.hd tl')
        | "=" -> Term.mk_eq tl'
        | "<=" -> Term.mk_leq tl'
        | "<" -> Term.mk_lt tl'
        | ">=" -> Term.mk_geq tl'
        | ">" -> Term.mk_gt tl'
        | "-" -> Term.mk_minus tl'
        | "+" -> Term.mk_plus tl'
        | "*" -> Term.mk_times tl'
        | "//" -> Term.mk_div tl'
        | "/" -> Term.mk_intdiv tl'
        | "mod" -> (
            match tl' with
            | term1 :: term2 :: [] -> Term.mk_mod term1 term2
            | _ -> assert false
        )
        | "abs" -> Term.mk_abs (List.hd tl')
        | _ -> assert false
    )
    | Ast.AttributeTerm (pos, term, _) -> generate_full_expr ref_list svi_map term 
    | Ast.True _ -> Term.mk_true ()
    | Ast.False _ -> Term.mk_false ()
    | Ast.Integer (_, int) -> Term.mk_num_of_int int
    | Ast.Real (_, real) -> Term.mk_num_of_int (int_of_float real) (* TODO: figure out how to handle reals in internal system *)
    | Ast.Let (pos, vbl, term) -> (
        let create_var_term_bind vb = (
            match vb with
            | Ast.VarBind (_, id, term') -> (
                let processed_term = generate_full_expr ref_list svi_map term' in
                let var = Var.mk_fresh_var (Term.type_of_term processed_term) in
                (var, processed_term)
            )
        ) in
        let create_rflist_items vb = (
            match vb with
            | Ast.VarBind (_, id, term') -> (
                let pt_type = Term.type_of_term (generate_full_expr ref_list svi_map term') in
                let rt = if Type.is_bool pt_type then "Bool"
                         else if Type.is_int pt_type then "Int"
                         else "Real"
                in
                (id, Ast.Sort(pos, rt), term)
            )
        ) in
        let ref_list' = (List.map create_rflist_items vbl) @ ref_list in
        let term' = generate_full_expr ref_list' svi_map term in
        let vars = List.map create_var_term_bind vbl in
        Term.mk_let vars term'
    )

let rec find_spec_exprs expr_list svi_map = 
    let ref_list = 
        filter_map 
        (fun x -> match x with Ast.DefineFun (_, id, [], rt, term) -> Some (id, rt, term) | _ -> None) 
        expr_list 
    in
    
    let init_expr =
        let init_check x = 
            match x with 
            | (id, rt, Ast.AttributeTerm (pos, term', prop_list)) -> (
                let find_init = fun x -> match x with Ast.InitTrue _ -> true | _ -> false in
                match find_opt find_init prop_list with
                | Some _ -> Some (id, generate_full_expr ref_list svi_map term')
                | None -> None
            )
            | _ -> None
        in
        filter_map init_check ref_list |> List.hd
    in

    let trans_expr =
        let trans_check x = 
            match x with 
            | (id, rt, Ast.AttributeTerm (pos, term', prop_list)) -> (
                let find_trans = fun x -> match x with Ast.TransTrue _ -> true | _ -> false in
                match find_opt find_trans prop_list with
                | Some _ -> Some (id, generate_full_expr ref_list svi_map term')
                | None -> None
            )
            | _ -> None
        in
        filter_map trans_check ref_list |> List.hd
    in

    let properties = 
        let prop_status = P.PropUnknown in
        let prop_check x = 
            match x with 
            | (_, rt, Ast.AttributeTerm (pos, term', prop_list )) -> (
                let find_invar = fun x -> match x with Ast.InvarProperty _ -> true | _ -> false in
                match find_opt find_invar prop_list with
                | None -> None
                | Some (Ast.InvarProperty (_, prop_num)) -> (        
                    let lib_pos = pos_of_file_row_col (pos.Position.fname, pos.Position.line, pos.Position.col) in
                    let prop_term = generate_full_expr ref_list svi_map term' in
                    let prop_source = P.PropAnnot (lib_pos) in
                    let prop_name = string_of_int prop_num in
                    let return_prop = 
                    {   P.prop_name; 
                        P.prop_source; 
                        P.prop_term; 
                        P.prop_status;  } 
                    in
                    Some (return_prop)
                )
                | _ -> assert false
            )
            | _ -> None
        in
        filter_map prop_check ref_list
    in
    init_expr, trans_expr, properties
    

let determine_var scope next_vars sort_map expr: StateVar.t option =
    match expr with
    | Ast.DeclareFun (pos, ident, [], sort) ->
    (
        let is_next = find_opt (fun x -> if fst x = ident then true else false) next_vars in
        match is_next with
        | Some (next_id, prev_id) -> None
        | None -> (
            let _type = (
                match sort with
                | Ast.Sort (_, "Bool") -> Type.mk_bool ()
                | Ast.Sort (_, "Int") -> Type.mk_int ()
                | Ast.Sort (_, "Real") -> Type.mk_real ()
                | Ast.Sort (_, str) -> (
                    match find_opt (fun x -> if (fst x) = str then true else false) sort_map with
                    | Some (_, "Bool") -> Type.mk_bool ()
                    | Some (_, "Int") -> Type.mk_int ()
                    | Some (_, "Real") -> Type.mk_real ()
                    | _ -> assert false
                ) 
                | _ -> assert false                
            ) 
            in
            let state_var = StateVar.mk_state_var ident scope _type in
            Some state_var
        )
    )
    | _ -> None

let determine_next expr : (string * string) option =
    match expr with
    | Ast.DefineFun (pos, ident, [], sort, Ast.AttributeTerm (_, Ast.Ident (_, prev_id), prop_list)) -> (  
        let find_next = fun x -> match x with Ast.NextName (_, next_id) -> Some next_id | _ -> None in
            match filter_map find_next prop_list with
            | [] -> None
            | next_id :: tail -> Some (next_id, prev_id)
    )
    | _ -> None

let create_map_instances_and_vars expr_list scope sort_map =
    let next_vars = expr_list |> filter_map (determine_next) in
    let state_vars = expr_list |> filter_map (determine_var scope next_vars sort_map) in
    let state_var_instance_map = (
        List.map (fun x -> (StateVar.name_of_state_var x, Var.mk_state_var_instance x Numeral.zero)) state_vars
        @ List.map (fun x -> (fst x, Var.mk_state_var_instance (List.find (fun y -> snd x = StateVar.name_of_state_var y) state_vars) Numeral.one)) next_vars
    )
    in
    state_vars, state_var_instance_map
    

let trans_sys_of_vmt 
    ?(preserve_sig = false)
    ?(slice_nodes = true)
    subsystem analysis_param
    =

    let expr_list = 
        SubSystem.all_subsystems subsystem
        |> List.map (function { SubSystem.source } -> source) 
        |> List.hd
    in

    let sort_map = filter_map (fun x -> match x with | Ast.DefineSort (_, ident, _, Ast.Sort(_, sort)) -> Some (ident, sort) | _ -> None) expr_list in

    let scope = ["Main"] in (* TODO: determine how to get the name for the scope *)

    let state_vars, svi_map = create_map_instances_and_vars expr_list scope sort_map in

    let init_expr, trans_expr, properties = find_spec_exprs expr_list svi_map in
    let init_term : Term.t = snd init_expr in
    let trans_term : Term.t = snd trans_expr in
              (* Filter assumptions for this node's assumptions *)
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

    let init_flag = StateVar.mk_init_flag ["main_init_flag"] in

    let init_term = 
        Term.mk_let 
            [(Var.mk_state_var_instance init_flag TransSys.init_base, Term.mk_true ())] 
            init_term 
    in

    let trans_term = 
        Term.mk_let 
            [(Var.mk_state_var_instance init_flag TransSys.trans_base, Term.mk_false ())] 
            trans_term 
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

            ([init_term], [trans_term])

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
