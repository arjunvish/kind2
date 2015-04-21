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

module C = Clause

(* Frame is a trie of clauses *)
module F = Clause.ClauseTrie

let debug_assert = true

let deactivate_actlit = true

let dead_actlits = ref []

let debug_check_sat_assuming s fu fs l =
  assert (not (List.exists (fun a1 -> List.exists (fun a2 -> Term.equal a1 a2) !dead_actlits) l));
  SMTSolver.check_sat_assuming s fu fs l
      
let generalize_after_fwd_prop = true

let subsume_in_block = false

let subsume_in_fwd_prop = true
  
(* ********************************************************************** *)
(* Solver instances and cleanup                                           *)
(* ********************************************************************** *)


(* Solver instance if created *)
let ref_solver = ref None

  
(* Formatter to output inductive clauses to *)
let ppf_inductive_assertions = ref Format.std_formatter

  
(* Output statistics *)
let print_stats () = 

  Event.stat
    [Stat.misc_stats_title, Stat.misc_stats;
     Stat.pdr_stats_title, Stat.pdr_stats;
     Stat.smt_stats_title, Stat.smt_stats]


(* Cleanup before exit *)
let on_exit _ = 

  (* Stop all timers *)
  Stat.pdr_stop_timers ();
  Stat.smt_stop_timers ();

  (* Output statistics *)
  print_stats ();

  (* Delete solver instance if created *)
  (try 
     match !ref_solver with 
       | Some solver -> 
         SMTSolver.delete_instance solver; 
         ref_solver := None
       | None -> ()
   with 
     | e -> 
       Event.log L_error 
         "Error deleting solver: %s" 
         (Printexc.to_string e));

  (* Delete solvers in quantifier elimination*)
  QE.on_exit ()


(* ********************************************************************** *)
(* Exception raised in proof process                                      *)
(* ********************************************************************** *)


(* All remaining properties are valid *)
exception Success of int

(* Counterexample trace for some property *)
exception Counterexample of C.t list 

(* Property disproved by other module *)
exception Disproved of string

(* Restart for other reason *)
exception Restart



(* ********************************************************************** *)
(* Utility functions                                                      *)
(* ********************************************************************** *)

(* Receive and handle events 

   Assert new invariants received and terminate on message *)
let handle_events
    solver
    trans_sys
    props =

  (* Receive queued messages 

     Side effect: Terminate when ControlMessage TERM is received.*)
  let messages = Event.recv () in

  (* Update transition system from messages *)
  let invariants_recvd, prop_status = 
    Event.update_trans_sys trans_sys messages 
  in

  (* Add invariant to the transition system and assert in solver
     instances *)
  let add_invariant inv = 

    match Term.var_offsets_of_term inv with
        
      (* Skip invariants without variables *)
      | None, None ->

        SMTSolver.trace_comment 
          solver
          "handle_event: Skipping constant invariant"

      (* One-state invariants *)
      | Some i, Some j when Numeral.equal i j ->

        SMTSolver.trace_comment 
          solver
          "handle_event: Asserting one-state invariant at zero and one";

        (* Assert at offset zero *)
        Term.bump_state Numeral.(- i) inv
        |> SMTSolver.assert_term solver;
        
        (* Assert at offset one *)
        Term.bump_state Numeral.(- i + one) inv
        |> SMTSolver.assert_term solver

      (* Two-state invariant *)
      | Some i, Some j when Numeral.(j - i = one) ->

        SMTSolver.trace_comment 
          solver
          "handle_event: Asserting two-state invariant at one";

        (* Assert at offset one *)
        Term.bump_state Numeral.(- i) inv |> SMTSolver.assert_term solver

      (* Invariant over more than two states *)
      | _ ->

        SMTSolver.trace_comment 
          solver
          "handle_event: Invariant is over more than two states";
        
        assert false
        
  in

  (* Assert all received invariants *)
  List.iter (fun i -> add_invariant i) invariants_recvd;

  (* Restart if one of the properties to prove has been disproved *)
  List.iter
    (fun (p, _) -> match TransSys.get_prop_status trans_sys p with 
       | TransSys.PropFalse _ -> raise (Disproved p)
       | _ -> ())
    props


(* Compute the frame sizes of a delta-encoded list of frames *)
let frame_sizes frames = 

  let rec aux accum = function 

    (* All frames added up, return accumulator *)
    | [] -> accum

    (* Take first frame *)
    | f :: tl -> 

      match accum with 

        (* No previous frame: take size of frame *)
        | [] -> aux [F.cardinal f] tl

        (* Add size of frame to size of previous frame *)
        | h :: _ -> aux (((F.cardinal f) + h) :: accum) tl

  in

  (* Start with empty accumulator *)
  aux [] frames


(* Compute the frame sizes of a delta-encoded list of frames in a
   blocking trace *)
let frame_sizes_block frames trace = 

  (* Frames with frames from trace *)
  let frames' = List.rev_append (List.map snd trace) frames in

  frame_sizes frames'

    
(* ************************************************************************ *)
(* Soundness check                                                          *)
(* ************************************************************************ *)

(* Check if for two successive frames R_i-1 & T |= R_i *)
let rec check_frames' solver prop_set accum = function

  | [] -> true

  | (r_i : C.t F.t) :: tl ->

    (* Check if all successors of frame are in the next frame *)
    let is_rel_ind () = 

      SMTSolver.trace_comment 
        solver
        (Format.sprintf 
           "check_frames: Does R_%d & T |= R_%d hold?"
           (List.length tl)
           (List.length tl |> succ));

      (* Activation literal for conjunction of clauses *)
      let actlit_n1 = 
        C.create_and_assert_fresh_actlit 
          solver
          "check_frames" 
          (List.map
             C.term_of_clause
             ((C.clause_of_prop_set prop_set) :: (F.values r_i) @ accum) |> Term.mk_and)
          C.Actlit_n1
      in

      (* Check P[x] & R_i-1[x] & T[x,x'] |= R_i[x'] & P[x'] *)
      SMTSolver.check_sat_assuming solver

        (* Fail if entailment does not hold *)
        (fun () -> false)

        (* Check preceding frames if entailment holds *)
        (fun () ->

          (* Deactivate activation literal *)
          if deactivate_actlit then 
            (Term.mk_not actlit_n1 |> SMTSolver.assert_term solver;
             Stat.incr Stat.pdr_stale_activation_literals;
             dead_actlits := actlit_n1 :: !dead_actlits);
          
          check_frames' solver prop_set ((F.values r_i) @ accum) tl)


        ((* Clauses of R_i are on rhs of entailment *)
          actlit_n1 ::

          (match tl with 

            (* Preceding frame is not R_0 *)
            | r_pred_i :: _ -> 

              C.actlit_p0_of_prop_set prop_set :: 
              
              List.map C.actlit_p0_of_clause accum @ 

              (* Clauses of R_i are in R_i-1, assert on lhs of entailment *)     
              List.map C.actlit_p0_of_clause (F.values r_i) @ 

              (* Assert clauses of R_i-1 on lhs of entailment *)
              List.map C.actlit_p0_of_clause (F.values r_pred_i)

            (* Preceding frame is R_0, assert initial state only *)
            | [] -> [C.actlit_of_frame 0]))

    in

    (* Check if all clauses in frame are initial *)
    let rec is_initial = function 

      (* Check if first clause is initial *)
      | c :: ctl -> 

        SMTSolver.trace_comment 
          solver
          (Format.sprintf 
             "check_frames: Does I |= C for C in R_%d hold?"
             (List.length tl |> succ));

        (* Check if clause is initial *)
        debug_check_sat_assuming 
          solver

          (* If sat: Clause is not initial *)
          (fun () -> false)

          (* If unsat: continue with next clause *)
          (fun () -> is_initial ctl)

          (* Check I |= C *)
          ((C.actlit_of_frame 0) :: C.actlits_n0_of_clause c)

      (* All clauses are initial, now check if frame successors of
         frame are in the next frame *)
      | [] -> is_rel_ind ()

    in

    SMTSolver.trace_comment 
      solver
      (Format.sprintf 
         "check_frames: Does R_%d |= P hold?"
         (List.length tl |> succ));

    SMTSolver.check_sat_assuming
      solver

      (* If sat: property is not implied by frame *)
      (fun () -> is_initial (F.values r_i))

      (* If unsat: Check if clauses are initial, then check if
         successors of frame are in the next frame *)
      (fun () -> is_initial (F.values r_i))

      (* Check R_i |= P *)
      (C.actlits_n0_of_prop_set prop_set @
         List.map C.actlit_p0_of_clause ((F.values r_i) @ accum))


let check_frames solver prop_set clauses frames =

  SMTSolver.trace_comment
    solver
    (Format.asprintf
       "@[<v>check_frames:@,%a@]"
       (pp_print_list
          (fun ppf r_i -> 
            Format.fprintf ppf
              "@,Frame:@,%a"
              (pp_print_list
                 (fun ppf c ->
                   Format.fprintf ppf
                     "%a"
                     Term.pp_print_term (C.actlit_p0_of_clause c))
                 "@,")
              (F.values r_i))
          "@,")
       frames);
  
  check_frames' solver prop_set clauses frames 


let order_terms terms term_tbl =

  List.sort 
    (fun t1 t2 -> 
       let v1 = 
         try Term.TermHashtbl.find term_tbl t1 with Not_found -> 0 
       in
       let v2 = 
         try Term.TermHashtbl.find term_tbl t2 with Not_found -> 0 
       in
       v1 - v2)
    terms


let incr_binding term term_tbl =

  let v = 

    try

      Term.TermHashtbl.find term_tbl term

    with Not_found -> 

      0 

  in

  Term.TermHashtbl.add term_tbl term (v+1)


(* ************************************************************************ *)
(* Utility functions for subsumption                                        *)
(* ************************************************************************ *)

(* Return the number of subsumed clauses and increment statistics 

   Pipe the result of Trie.S.subsume through this function. *)
let count_subsumed solver (c, f) =

  (* Number of subsumed clauses *)
  let num = List.length c in

  (* Only if at least one clause subsumed *)
  if num > 0 then
  
    (SMTSolver.trace_comment
       solver
       (Format.sprintf
          "@[<v>Backward subsumed %d clauses in R_k@]" num);

     (* Increment statistics *)
     Stat.incr ~by:num Stat.pdr_back_subsumed);

  (* Return result unchanged *)
  (c, f)
    
    
(* Deactivate activation literals of a subsumed clause *)
let deactivate_clause solver clause = 

  SMTSolver.trace_comment
    solver
    "Deactivating activation literals for clause";
      
  (* Assert negation of activation literal for positive unprimed
     clause *)
  C.actlit_p0_of_clause clause
  |> Term.mk_not
  |> SMTSolver.assert_term solver;

  dead_actlits := C.actlit_p0_of_clause clause :: !dead_actlits;
  
  (* Assert negation of activation literal for positive primed
     clause *)
  C.actlit_p1_of_clause clause
  |> Term.mk_not
  |> SMTSolver.assert_term solver;
  
  dead_actlits := C.actlit_p0_of_clause clause :: !dead_actlits;

  (* Assert negation of activation literals for negated unprimed
     clause literals *)
  C.actlits_n0_of_clause clause
  |> List.iter (fun t -> Term.mk_not t |> SMTSolver.assert_term solver);

  List.iter
    (fun l -> dead_actlits := l :: !dead_actlits)
    (C.actlits_n0_of_clause clause);

  (* Assert negation of activation literals for negated primed
     clause literals *)
  C.actlits_n1_of_clause clause
  |> List.iter (fun t -> Term.mk_not t |> SMTSolver.assert_term solver);

  List.iter
    (fun l -> dead_actlits := l :: !dead_actlits)
    (C.actlits_n1_of_clause clause);

  (* Increment statistics for activation literals *)
  Stat.incr
    ~by:(2 + 2 * (List.length (C.literals_of_clause clause)))
    Stat.pdr_stale_activation_literals

    
(* Deactivate activation literals of subsumed clauses *)
let deactivate_subsumed solver (subsumed, frame') =

  (* Check flag *)
  if deactivate_actlit then

    (* Deactivate activation literals for each subsumed clause *)
    (List.iter
       (fun (_, c) -> deactivate_clause solver c)
       subsumed);
  
  (subsumed, frame')


(* ************************************************************************ *)
(* Inductive generalization                                                 *)
(* ************************************************************************ *)
    
(* Inductively generalize [clause] relative to [frame]

   Assuming that [clause] is relatively inductive to [frame] and
   initial, find a smaller subclause of [clause] that is still
   relatively inductive to [frame] and initial. *)
let ind_generalize solver prop_set frame clause literals =

  (* Linearly traverse the list of literals in the clause, and remove
     a literal the clause without the literal remains relatively
     inductive and initial

     [kept] are the literals that cannot be removed. 

  *)
  let rec linear_search kept = function

    (* All literals considered, return literals that had to be kept *)
    | [] ->

      (* Could we drop literals? *)
      if List.length kept = C.length_of_clause clause then 

        (* Return clause unchanged *)
        clause

      else

        (

          SMTSolver.trace_comment solver
            (Format.sprintf 
               "ind_generalize: Dropped %d literals from clause."
               (C.length_of_clause clause - List.length kept));

          (* Deactivate activation literal of parent clause *)
          if deactivate_actlit then
            deactivate_clause solver clause;
            
          (* New clause with generalized clause as parent *)
          C.clause_of_literals solver (Some clause) kept

        )

    (* Do not try to generalize to the empty clause, this should not
       be possible in a sound transition system *)
    | l :: [] when kept = [] -> linear_search [l] []
        
    | l :: tl ->

      (* Clause without current literal *)
      let clause' = kept @ tl |> Term.mk_or in

      (* Actiation literal for clause *)
      let clause'_actlit_p0, clause'_actlit_n0, clause'_actlit_n1 = 
        let mk = C.create_and_assert_fresh_actlit solver "ind_gen" clause' in 
        mk C.Actlit_p0, mk C.Actlit_n0, mk C.Actlit_n1
      in

      (* Keep literal and try with following literals *)
      let keep_literal () =

        (* Deactivate activation literal *)
        if deactivate_actlit then 
          (Term.mk_not clause'_actlit_p0 |> SMTSolver.assert_term solver;
           Term.mk_not clause'_actlit_n0 |> SMTSolver.assert_term solver;
           Term.mk_not clause'_actlit_n1 |> SMTSolver.assert_term solver;

           dead_actlits :=
             clause'_actlit_p0 :: clause'_actlit_n0 :: clause'_actlit_n1 :: !dead_actlits;
           
           Stat.incr ~by:3 Stat.pdr_stale_activation_literals);
          
        linear_search (l :: kept) tl

      in

      (* Drop literal and try with following literals *)
      let drop_literal () =

        (* Deactivate activation literal *)
        if deactivate_actlit then 
          (Term.mk_not clause'_actlit_p0 |> SMTSolver.assert_term solver;
           Term.mk_not clause'_actlit_n0 |> SMTSolver.assert_term solver;
           Term.mk_not clause'_actlit_n1 |> SMTSolver.assert_term solver;

           dead_actlits :=
             clause'_actlit_p0 :: clause'_actlit_n0 :: clause'_actlit_n1 :: !dead_actlits;
           
           Stat.incr ~by:3 Stat.pdr_stale_activation_literals);
          
        linear_search kept tl

      in

      (* Clause without literal is initial *)
      let is_initial () = 

        SMTSolver.trace_comment solver
          "ind_generalize: Checking if clause without literal is \
           relatively inductive.";

        debug_check_sat_assuming 
          solver

          (* If sat: Clause without literal is not relatively inductive *)
          keep_literal

          (* If unsat: Clause without literal is relatively inductive *)
          drop_literal

          (* Check P[x] & R[x] & C[x] & T[x,x'] |= C[x'] *)
          (C.actlit_p0_of_prop_set prop_set ::
             clause'_actlit_p0 ::
             clause'_actlit_n1 ::
             frame)

      in

      SMTSolver.trace_comment solver
        "ind_generalize: Checking if clause without literal is initial.";

      debug_check_sat_assuming 
        solver

        (* If sat: Clause without literal is not initial *)
        keep_literal

        (* If unsat: Clause without literal is initial *)
        is_initial 

        (* Check I |= C *)
        ([clause'_actlit_n0; C.actlit_of_frame 0])


  in

  linear_search [] literals

(*


      let kept_woc = C.remove c kept in

  let block_term = C.to_term kept_woc in
  let primed_term = Term.mk_and (List.map (fun t -> Term.negate (Term.bump_state Numeral.one t)) (C.elements kept_woc)) in

  let init = SMTSolver.check_sat_term solver_init [Term.negate block_term] in
  let (cons, model) = SMTSolver.check_sat_term_model solver_frames [(Term.mk_and [block_term;primed_term])] in

  (* If, by removing the literal c, the blocking clause then
       either a. becomes reachable in the inital state or b. satisfies
       consecution then we need to keep it *)
  if cons || init then 

    (debug pdr
           "@[<v>%a@]"
           (pp_print_list 
              (fun ppf (v, t) -> 
               Format.fprintf ppf 
                              "(%a %a)"
                              Var.pp_print_var v
                              Term.pp_print_term t)
              "@,")
           model
     in

     linear_search kept discarded cs)

  else (

    debug pdr "Removing literal: %a" Term.pp_print_term c in

    incr_binding c term_tbl;

    Stat.incr Stat.pdr_literals_removed;

    linear_search kept_woc (c :: discarded) cs

  )
  | [] ->  kept, C.of_literals discarded
                                    

                                    
    in

    let binary_search kept clause =
      
      let discarded = ref [] in
      
      let rec binary_search kept clause =
        let block_term = C.to_term (C.of_literals kept) in
        let primed_term = Term.mk_and (List.map (fun t -> Term.bump_state Numeral.one (Term.negate t)) kept) in
        
        let init = SMTSolver.check_sat_term solver_init [Term.negate block_term] in
        let cons = SMTSolver.check_sat_term solver_frames [(Term.mk_and [block_term;primed_term])] in
        
        if not (cons || init) then (
          discarded := !discarded @ (Array.to_list clause);
          []
        )
        else if Array.length clause < 2 then
          Array.to_list clause
        else
          let m = (Array.length clause) / 2 in
          let t1 = Array.sub clause 0 (m/2) in
          let t2 = Array.sub clause ((m/2)+1) m in
          let m2 = binary_search (kept @ (Array.to_list t1)) t2 in
          let m1 = binary_search (kept @ m2) t1 in      
          m1 @ m2
      in
      
      (C.of_literals (binary_search kept clause)), (C.of_literals !discarded)

    in


    
    let block_term = C.to_term clause in
    let primed_term = Term.mk_and (List.map (fun t -> Term.negate (Term.bump_state Numeral.one t)) (C.elements clause)) in

    let init = SMTSolver.check_sat_term solver_init [Term.negate block_term] in
    let (cons,model) = SMTSolver.check_sat_term_model solver_frames [(Term.mk_and [block_term;primed_term])] in

    (debug pdr
           "@[<v>%a@]"
           (pp_print_list 
              (fun ppf (v, t) -> 
               Format.fprintf ppf 
                              "(%a %a)"
                              Var.pp_print_var v
                              Term.pp_print_term t)
              "@,")
           model
     in

     assert (not cons));
    
    let k,d = match Flags.pdr_inductively_generalize() with
      | 1 -> linear_search clause [] (C.elements clause)
      | 2 -> linear_search clause [] (order_terms (C.elements clause) term_tbl)
      | 3 -> binary_search [] (Array.of_list (C.elements clause))
      | _ -> clause , C.empty
    in



    debug pdr
          "@[<v>Reduced blocking clause to@,@[<v>%a@]"
          (pp_print_list Term.pp_print_term "@,") 
          (C.elements k)
    in

    k,d

*)


(* ************************************************************************ *)
(* Extrapolation from a two-state counterexample                            *)
(* ************************************************************************ *)

(* Given a model and two formulas f and g return a conjunction of
   literals such that 

   (1) x = s |= B[x] 
   (2) B[x] |= exists x' (F[x] & T[x,x'] & G[x']) *)
let extrapolate trans_sys state f g = 

  (* Construct term to be generalized with the transition relation and
     the invariants *)
  let term = 
    Term.mk_and 
      [f; 
       TransSys.trans_of_bound trans_sys Numeral.one; 
(*
       TransSys.invars_of_bound trans_sys ~one_state_only:true Numeral.zero; 
       TransSys.invars_of_bound trans_sys Numeral.one; 
*)
       Term.bump_state Numeral.one g]
  in

  (* Get primed variables in the transition system *)
  let primed_vars = 
    Var.VarSet.elements
      (Term.vars_at_offset_of_term (Numeral.one) term) 
  in 

  Stat.start_timer Stat.pdr_generalize_time;

  (* Generalize term by quantifying over and eliminating primed
     variables *)
  let gen_term = 
    QE.generalize 
      trans_sys
      (TransSys.uf_defs trans_sys) 
      state
      primed_vars
      term 
  in

  Stat.record_time Stat.pdr_generalize_time;

  (* Return generalized term *)
  gen_term


(* ************************************************************************ *)
(* Block unreachable generalized counterexamples to induction               *)
(* ************************************************************************ *)

(* Add cube to block in future frames *)
let add_to_block_tl solver block_clause block_trace = function

  (* Last frame has no successors *)
  | [] -> [] 

  (* Add cube as proof obligation in next frame *)
  | (block_clauses, r_succ_i) :: block_clauses_tl -> 

    (* Copy clause with a new activation literal *)
    let copy_clause c =

      (* Get parent of clause *)
      let p = C.parent_of_clause c in

      (* Return no parent if clause is its own parent *)
      let c_p =
        if Term.equal (C.actlit_p0_of_clause p) (C.actlit_p0_of_clause c) then
          None
        else
          (Some p)
      in

      (* Create a new clause with the same literals *)
      C.literals_of_clause c |> C.clause_of_literals solver c_p
          
    in
    
(*    (block_clauses @ [copy_clause block_clause, block_trace], r_succ_i) :: block_clauses_tl *)
    ((copy_clause block_clause, block_trace) :: block_clauses, r_succ_i) :: block_clauses_tl


(* Block sets of bad states in frames

   The last two arguments [frames] and [trace] are lists of frames and
   cubes to block. 

   [frames] is the list of frames below the current frame in
   descencing order, with R_i-1 at the head and R_1 last. 

   [trace] is the list of frames above the current frame in ascending
   order with R_i at the head and R_k last. Each frame is paired with
   a list of cubes that are to be shown unreachable in that frame.

*)
let rec block solver trans_sys prop_set term_tbl = 

  function 

    (* Nothing to block in frames above, current frame is R_k *)
    | [] -> 

      (function 

        (* k > 0, we must have at least one frame *)
        | [] ->  raise (Invalid_argument "block")

        (* Head of frames is the last frame *)
        | r_k :: frames_tl as frames -> 

          (* All successors of R_k are safe *)
          let r_k_is_safe () = 

            SMTSolver.trace_comment 
              solver
              (Format.sprintf 
                 "block: All successors of R_%d are safe."
                 (List.length frames));

            (* Return frames *)
            frames

          in

          (* We can violate the property in one step from R_k *)
          let block_in_r_k () = 

            (* Get counterexample as a pair of states from satisfiable
               query *)
            let cti =

              (* This is expensive due to the many activation literals *)
              (* SMTSolver.get_model solver *)
              
              SMTSolver.get_var_values
                solver
                (TransSys.vars_of_bounds trans_sys Numeral.zero Numeral.one)
                
            in

            (* Extrapolate from counterexample to a cube in R_k

               P[x] & R_k[x] & T[x,x'] & ~P[x'] is sat

               R_k does not imply P[x] yet *)
            let cti_gen = 
              extrapolate 
                trans_sys 
                cti 
                (C.term_of_prop_set prop_set :: 
                   List.map C.term_of_clause (F.values r_k)
                    |> Term.mk_and)
                (C.term_of_prop_set prop_set |> Term.negate) 
            in

            (* Create a clause with activation literals from generalized
               counterexample *)
            let clause = 
              C.clause_of_literals solver None (List.map Term.negate cti_gen) 
            in

            (* Recursively block cube by showing that clause is
               relatively inductive *)
            block 
              solver
              trans_sys 
              prop_set
              ()
              [([clause, [C.clause_of_prop_set prop_set]], r_k)] 
              frames_tl

          in

          (* Receive and assert new invariants *)
          handle_events solver trans_sys (C.props_of_prop_set prop_set);

          SMTSolver.trace_comment 
            solver
            (Format.sprintf 
               "block: Check if all successors of frontier R_%d are safe."
               (List.length frames));

          debug_check_sat_assuming 
            solver 

            (* If sat: we have a state in R_k that has a successor
               outside the property *)
            block_in_r_k

            (* If unsat: Frames are safe, cannot get outside property
               in one step in all frames up to R_k *)
            r_k_is_safe

            (* Check P[x] & R_k[x] & T[x,x'] |= P[x']

               R_k does not imply P[x] yet *)
            (C.actlit_p0_of_prop_set prop_set :: 
               C.actlits_n1_of_prop_set prop_set @
               List.map C.actlit_p0_of_clause (F.values r_k))

      )

    (* No more cubes to block in R_i *)
    | ([], r_i) :: block_tl -> 

      (function frames ->

        SMTSolver.trace_comment 
          solver
          (Format.sprintf 
             "block: All counterexamples blocked in R_%d"
             (succ (List.length frames)));

        (* Return to counterexamples to block in R_i+1 *)
        block 
          solver
          trans_sys
          prop_set
          term_tbl
          block_tl
          (r_i :: frames))


    (* Take the first cube to be blocked in current frame *)
    | (((block_clause, block_trace) :: block_clauses_tl), r_i) 
      :: block_tl as trace -> 

      (function frames -> 

        (* Combine clauses from higher frames to get the actual
           clauses of the delta-encoded frame R_i-1

           Get clauses in R_i..R_k from [trace], R_i-1 is first frame
           in [frames]. *)
        let clauses_r_succ_i, actlits_p0_r_succ_i = 
          List.fold_left
            (fun (ac, al) (_, r) ->
              (F.values r) @ ac, List.map C.actlit_p0_of_clause (F.values r) @ al)
            ([], [])
            trace
        in

(*
        let clauses_r_pred_i, actlits_p0_r_pred_i = 

          List.fold_left

            (* Join lists of clauses *)
            (fun (ac, al) (_, r) ->
              (F.values r) @ ac, List.map C.actlit_p0_of_clause (F.values r) @ al)

            (* May be empty *)
            ((match frames with 
              (* Special case: R_0 = I *)
              | [] -> ([], [C.actlit_of_frame 0])
              | r_pred_i :: _ -> 
                (C.clause_of_prop_set prop_set :: (F.values r_pred_i), 
                 C.actlit_p0_of_prop_set prop_set :: 
                   List.map C.actlit_p0_of_clause (F.values r_pred_i))))
            
            trace

        in
*)

        (* Combine clauses from higher frames to get the actual
           clauses of the delta-encoded frame R_i-1

           Get clauses in R_i..R_k from [trace], R_i-1 is first frame
           in [frames]. *)
        let clauses_r_pred_i, actlits_p0_r_pred_i = 

          (* May be empty *)
          match frames with
              
            (* Special case: R_0 = I *)
            | [] -> ([], [C.actlit_of_frame 0])

            | r_pred_i :: _ -> 
              
              List.fold_left
                
                (* Join lists of clauses *)
                (fun (ac, al) (_, r) ->
                  (F.values r) @ ac, List.map C.actlit_p0_of_clause (F.values r) @ al)
                
                (C.clause_of_prop_set prop_set :: (F.values r_pred_i), 
                 C.actlit_p0_of_prop_set prop_set :: 
                   List.map C.actlit_p0_of_clause (F.values r_pred_i))
            
                trace

        in
        
        (* Clause is relative inductive to this frame *)
        let is_rel_inductive () = 

          (* Activation literals in unsat core of query *)
          let core_actlits_trans = SMTSolver.get_unsat_core_lits solver in

          SMTSolver.trace_comment 
            solver
            "block: Check I |= C to get unsat core.";
          
          (* Activation literals in unsat core of I |= C *)
          let core_actlits_init = 
            debug_check_sat_assuming
              solver

              (* Must be unsat *)
              (fun () -> 

                Event.log L_info "Query is satisfiable, waiting for BMC";

                (* This should only happen when we are faster than
                   BMC, who has not yet discovered at one-step
                   violation of a property. We wait for messages *)
                let rec wait () = 
                  handle_events 
                    solver
                    trans_sys
                    (C.props_of_prop_set prop_set);
                  minisleep 0.01;
                  wait ()
                in
                ignore (wait ()); 

                (* We won't return from waiting *)
                assert false)

              (* Get literals in unsat core *)
              (fun () -> SMTSolver.get_unsat_core_lits solver)
              
              (* Check I |= C *)
              ((C.actlit_of_frame 0) :: C.actlits_n0_of_clause block_clause)
              
          in
          
          (* Reduce clause to unsat core of R & T |= C *)
          let block_clause_literals_core_n1 = 

            List.fold_left2 

              (fun a t l ->

                if 

                  (* Keep clause literal [l] if activation literals
                     [t] is in unsat core *)
                  List.exists (Term.equal t) core_actlits_trans
                    
                then

                  l :: a

                else

                  a)

              (* Start with empty clause *)
              []

              (* Fold over clause literals and their activation literals *)
              (C.actlits_n1_of_clause block_clause)
              (C.literals_of_clause block_clause)

          in
          
          (* Reduce clause to unsat core of I |= C *)
          let block_clause_literals_core = 

            List.fold_left2 

              (fun a t l ->

                if 

                  (* Keep clause literal [l] if activation literal [t]
                     is in unsat core *)
                  List.exists (Term.equal t) core_actlits_init
                    
                then

                  (* Drop clause literal [l] if it is in accumulator
                     to prevent duplicates *)
                  if List.exists (Term.equal l) a then a else l :: a
                    
                else
                  
                  a)

              (* Start with literal in core of consecution query *)
              block_clause_literals_core_n1

              (* Fold over clause literals and their activation literals *)
              (C.actlits_n0_of_clause block_clause)
              (C.literals_of_clause block_clause)

          in
          
          SMTSolver.trace_comment
            solver
            (Format.asprintf
               "@[<hv>block: Reduced clause@ %a@ with unsat core to@ %a@]"
               Term.pp_print_term (C.term_of_clause block_clause)
               Term.pp_print_term (Term.mk_or block_clause_literals_core));
          
          (* Inductively generalize clause *)
          let block_clause_gen =
            Stat.time_fun Stat.pdr_ind_gen_time
              (fun () -> 
                ind_generalize 
                  solver
                  prop_set
                  actlits_p0_r_pred_i
                  block_clause
                  block_clause_literals_core)
          in

          SMTSolver.trace_comment
            solver
            (Format.asprintf
               "@[<hv>block: Reduced clause@ %a@ with ind. gen. to@ %a@]"
               Term.pp_print_term (Term.mk_or block_clause_literals_core)
               Term.pp_print_term (C.term_of_clause block_clause_gen));
          
          (* Add blocking clause to all frames up to where it has to
             be blocked *)
          let r_i', frames', block_tl' =

            (* Literals of clause as key for trie *)
            let block_clause_gen_literals = C.literals_of_clause block_clause_gen in

            (* Subsumption for each new blocking clause is expensive *)
            if subsume_in_block then
              
              if

                (* Don't add clause to frame if it is subsumed in R_i or
                   any preceding frame *)
                F.is_subsumed r_i block_clause_gen_literals ||
                  List.exists
                  (fun f -> F.is_subsumed f block_clause_gen_literals)
                  frames

              then

                (deactivate_clause solver block_clause_gen;

                  Stat.incr Stat.pdr_back_subsumed;

                  SMTSolver.trace_comment
                   solver
                   (Format.asprintf
                      "@[<v>Clause@ @[<hv>{%a@}@] is subsumed in frame@ @[<hv>%a@]@]"
                      (pp_print_list Term.pp_print_term ";@ ")
                      block_clause_gen_literals
                      (pp_print_list
                         (fun ppf (k, c) ->
                           Format.fprintf ppf
                             "@[<hv 1>{%a}@ =@ %a"
                             (pp_print_list Term.pp_print_term ";@ ") k
                             Term.pp_print_term (C.term_of_clause c))
                         ",@ ")
                      (F.bindings r_i));

                 (* Drop blocking clause *)
                 r_i, frames, block_tl)

              else

                (SMTSolver.trace_comment
                   solver
                   "Adding clause to frame";
                 
                 (* Subsume in frame and add clauses *)
                 F.subsume r_i block_clause_gen_literals

                    (* Count number of subsumed clauses *)
                    |> count_subsumed solver

                    (* Deactivate activation literals of subsumed clauses *)
                    |> deactivate_subsumed solver

                    |> snd
                        
                    (* Add clause to frame after subsumption *)
                    |> F.add block_clause_gen_literals block_clause_gen,
                 
                 (* Subsume in preceding frames *)
                 List.map
                   (fun f ->
                     F.subsume f block_clause_gen_literals
                       |> count_subsumed solver
                       |> deactivate_subsumed solver
                       |> snd)
                   frames,

                 (* Add cube to block to next higher frame if flag is set *)
                 if 
                   
                   Flags.pdr_block_in_future ()
                     
                 then
                   
                   add_to_block_tl solver block_clause_gen block_trace block_tl
                     
                 else
                   
                   block_tl

                )
                  
            else
              
              (try
                 
                 (* Adding a clause may fail if it a prefix of a
                    clause in the trie, or if a clause in the trie is
                    a prefix of this clause *)
                 F.add block_clause_gen_literals block_clause_gen r_i
                   
               with Invalid_argument _ ->

                 (SMTSolver.trace_comment
                    solver
                    (Format.asprintf
                       "@[<v>Clause@ @[<hv>{%a@}@] subsumes a clause in frame, \
                        must do subsumption before adding@ @[<hv>%a@]@]"
                       (pp_print_list Term.pp_print_term ";@ ")
                       block_clause_gen_literals
                       (pp_print_list
                          (fun ppf (k, c) ->
                            Format.fprintf ppf
                              "@[<hv 1>{%a}@ =@ %a"
                              (pp_print_list Term.pp_print_term ";@ ") k
                              Term.pp_print_term (C.term_of_clause c))
                          ",@ ")
                       (F.bindings r_i));

                  (* The new blocking clause is not subsumed, because
                     otherwise we would not get the counterexample *)
                  (* if F.is_subsumed r_i block_clause_gen_literals then r_i else *)
                   
                  (* Subsume in this frame and add *)
                  F.subsume r_i block_clause_gen_literals

                     (* Count number of subsumed clauses *)
                     |> count_subsumed solver
                         
                     (* Deactivate activation literals of subsumed clauses *)
                     |> deactivate_subsumed solver

                     |> snd
                         
                     (* Add clause to frame after subsumption *)
                     |> F.add block_clause_gen_literals block_clause_gen)),
              
              frames,

              block_tl
                
          in

          (* DEBUG *)
          if debug_assert then
            assert
              (check_frames solver prop_set clauses_r_succ_i (r_i' :: frames'));
                    
          (* Update frame size statistics *)
          Stat.set_int_list
            (frame_sizes_block frames' trace) 
            Stat.pdr_frame_sizes; 

          (* Add clause to frame and continue with next clauses in
             this frame *)
          block 
            solver
            trans_sys 
            prop_set
            term_tbl
            ((block_clauses_tl, r_i') :: block_tl') 
            frames'

        in

        (* Clause is not relative inductive to this frame *)
        let block_in_r_i () =
          
          (* Are there frames below R_i? *)
          match frames with 
              
            (* Bad state is reachable from R_0, we have found a
               counterexample path *)
            | [] -> raise (Counterexample (block_clause :: block_trace))
              
            (* i > 1 and bad state is reachable from R_i-1 *)
            | r_pred_i :: frames_tl -> 
              
              (* Get counterexample from satisfiable query *)
              let cti =
                (* SMTSolver.get_model solver *)
                SMTSolver.get_var_values
                  solver
                  (TransSys.vars_of_bounds trans_sys Numeral.zero Numeral.one)
              in
              
              (* Generalize the counterexample to a list of literals
                 
                 R_i-1[x] & C[x] & T[x,x'] & ~C[x'] is sat *)
              let cti_gen = 
                extrapolate 
                  trans_sys 
                  cti
                  ((C.term_of_clause block_clause ::
                      List.map C.term_of_clause clauses_r_pred_i)
                      |> Term.mk_and)
                  (C.term_of_clause block_clause |> Term.negate)
              in
              
              (* Create a clause with activation literals from generalized
                 counterexample *)
              let block_clause' = 
                C.clause_of_literals solver None (List.map Term.negate cti_gen) 
              in
              
              block 
                solver
                trans_sys 
                prop_set
                term_tbl
                (([block_clause', (block_clause :: block_trace)], 
                  r_pred_i) :: trace) 
                frames_tl
                
        in

        (* Receive and assert new invariants *)
        handle_events solver trans_sys (C.props_of_prop_set prop_set);

        SMTSolver.trace_comment 
          solver
          (Format.sprintf 
             "block: Is blocking clause relative inductive to R_%d?"
             (List.length frames));

        debug_check_sat_assuming 
          solver
          
          (* If sat: bad state is reachable *)
          block_in_r_i

          (* If unsat: clause is relative inductive and bad state is
             not reachable *)
          is_rel_inductive

          (* Check P[x] & R_i-1[x] & C[x] & T[x,x'] |= C[x'] *)
          (C.actlit_p0_of_clause block_clause :: 
             C.actlits_n1_of_clause block_clause @
             actlits_p0_r_pred_i)

      )


(* ************************************************************************ *)
(* Forward propagation                                                      *)
(* ************************************************************************ *)

(* Split list of clauses into those that are inductive and those that
   are not *)
let rec partition_rel_inductive
    solver
    trans_sys
    frame
    not_inductive
    maybe_inductive = 
  
  (* Conjunction of clauses *)
  let clauses = Term.mk_and (List.map C.term_of_clause maybe_inductive) in

  (* Assert p0 => C_1 & ... & C_n *)
  let actlit_p0 = 
    C.create_and_assert_fresh_actlit solver "rel_ind" clauses C.Actlit_p0
  in

  (* Assert p0 => ~(C_1' & ... & C_n') *)
  let actlit_n1 = 
    C.create_and_assert_fresh_actlit solver "rel_ind" clauses C.Actlit_n1
  in

  (* All candidate clause are inductive: return clauses show not to be
     inductive and inductive clauses *)
  let all_clauses_inductive () =

    (* Deactivate activation literal *)
    if deactivate_actlit then 
      (Term.mk_not actlit_p0 |> SMTSolver.assert_term solver;
       Term.mk_not actlit_n1 |> SMTSolver.assert_term solver;

       dead_actlits :=
         actlit_p0 :: actlit_n1 :: !dead_actlits;
       
       Stat.incr ~by:2 Stat.pdr_stale_activation_literals);
        
    not_inductive, maybe_inductive in

  (* Some candidate clauses are not inductive: filter out the ones
     that could still be *)
  let some_clauses_not_inductive () =
    
    (* Get model for failed entailment check *)
    let model =
      (* SMTSolver.get_model solver *)
      SMTSolver.get_var_values
        solver
        (TransSys.vars_of_bounds trans_sys Numeral.zero Numeral.one)
    in
        
    (* Separate not inductive terms from potentially inductive terms 
       
       C_1 & ... & C_n & T & ~ (C_1' & ... & C_n') is satisfiable,
       partition C_1', ..., C_n' by their model value, false terms are
       certainly not inductive, true terms can be inductive. *)
    let maybe_inductive', not_inductive_new = 
      List.partition 
        (function c -> 
          C.term_of_clause c
          |> Term.bump_state Numeral.one
          |> Eval.eval_term [] model
          |> Eval.bool_of_value)
        maybe_inductive
    in

    (* Clauses found to be not inductive *)
    let not_inductive' = not_inductive @ not_inductive_new in
    
    (* Deactivate activation literal *)
    if deactivate_actlit then 
      (Term.mk_not actlit_p0 |> SMTSolver.assert_term solver;
       Term.mk_not actlit_n1 |> SMTSolver.assert_term solver;

       dead_actlits :=
         actlit_p0 :: actlit_n1 :: !dead_actlits;
       
       Stat.incr ~by:2 Stat.pdr_stale_activation_literals);
        
    (* No clauses are inductive? *)
    if maybe_inductive = [] then (not_inductive', []) else

      (* Continue checking if remaining clauses are inductive *)
      partition_rel_inductive 
        solver
        trans_sys 
        frame
        not_inductive'
        maybe_inductive'
        
  in

  SMTSolver.trace_comment
    solver
    "Checking for inductiveness of clauses";

  (* Are all clauses inductive? 

     Check R & C_1 & ... & C_n & T |= C_1' & ... & C_n'
  *)
  debug_check_sat_assuming 
    solver
    some_clauses_not_inductive 
    all_clauses_inductive
    (actlit_p0 :: actlit_n1 :: List.map C.actlit_p0_of_clause frame)
    


(* Split list of clauses into clauses that can be propagated relative
   to the frame and those that cannot be *)
let partition_fwd_prop
    solver
    trans_sys
    prop_set
    frame
    clauses = 

  (* Assert p0 => C_1 & ... & C_n

     Use the same activation literal on lhs for all checks *)
  let actlits_p0 =
    List.map C.actlit_p0_of_clause (frame @ clauses) 
  in

  (* Check until we find a set of clauses that can be propagated
     together *)
  let rec partition_fwd_prop' keep maybe_prop = 

    (* All clause can be forward propagated: return clauses to keep and
       clauses to propagate *)
    let prop_all actlit_n1 () =

      (* Deactivate activation literal *)
      if deactivate_actlit then 
        (Term.mk_not actlit_n1 |> SMTSolver.assert_term solver;

         dead_actlits := actlit_n1 :: !dead_actlits;
       
         Stat.incr Stat.pdr_stale_activation_literals);
      
      keep, maybe_prop

    in

    (* Some candidate clauses cannot be propagated: filter out the ones
       that could still be *)
    let keep_some actlit_n1 () =

      (* Get model for failed entailment check *)
      let model =
        (* SMTSolver.get_model solver *)
        SMTSolver.get_var_values
          solver
          (TransSys.vars_of_bounds trans_sys Numeral.zero Numeral.one)
      in

      (* Separate not propagateable terms from potentially propagateable
         terms

         C_1 & ... & C_n & T & ~ (C_1' & ... & C_n') is satisfiable,
         partition C_1', ..., C_n' by their model value, false terms are
         certainly not propagateable, true terms might be propagated. *)
      let maybe_prop', keep_new = 
        List.partition 
          (function c -> 
            C.term_of_clause c
            |> Term.bump_state Numeral.one
            |> Eval.eval_term [] model
            |> Eval.bool_of_value)
          maybe_prop
      in

      (* Clauses found not propagateable *)
      let keep' = keep @ keep_new in

      (* Deactivate activation literal *)
      if deactivate_actlit then 
        (Term.mk_not actlit_n1 |> SMTSolver.assert_term solver;

         dead_actlits := actlit_n1 :: !dead_actlits;
       
         Stat.incr Stat.pdr_stale_activation_literals);
        
      (* No clauses can be propagated? *)
      if maybe_prop' = [] then (keep', []) else

        (* Continue checking if remaining clauses are inductive *)
        partition_fwd_prop' 
          keep'
          maybe_prop'

    in

    SMTSolver.trace_comment
      solver
      "partition_fwd_prop: Checking for forward propagation of clause set";

    (* Assert n1 => ~(C_1' & ... & C_n') *)
    let actlit_n1 = 
      C.create_and_assert_fresh_actlit 
        solver
        "fwd_prop" 
        (List.map C.term_of_clause maybe_prop |> Term.mk_and) 
        C.Actlit_n1
    in

    (* Can all clauses be propagated? 

       Check P[x] & R[x] & T[x,x'] |= C_1[x'] & ... & C_n[x']
    *)
    debug_check_sat_assuming 
      solver
      (keep_some actlit_n1)
      (prop_all actlit_n1)
      (C.actlit_p0_of_prop_set prop_set :: actlit_n1 :: actlits_p0)
    
  in

  (* Check if all clauses can be propagated *)
  partition_fwd_prop' [] clauses

    
(* Forward propagate clauses in all frames *)
let fwd_propagate solver trans_sys prop_set frames = 

  let subsume_and_add a c =

    SMTSolver.trace_comment
      solver
      (Format.asprintf
         "@[<v>subsume_and_add: clause %a@]"
         Term.pp_print_term (C.actlit_p0_of_clause c));

    (* Forward propagated clause to add to frame *)
    let c' =

      (* Inductive generalization after forward propagation? *)
      if generalize_after_fwd_prop then
        
        (Stat.time_fun Stat.pdr_ind_gen_time
           (fun () -> 
             ind_generalize 
               solver
               prop_set
               (F.values a |> List.map C.actlit_p0_of_clause)
               c
               (C.literals_of_clause c)))
          
      else

        (* Propagate clause as it is *)
        c
          
    in

    (* Literals of clause as key for trie *)
    let l = C.literals_of_clause c' in

    (* Subsumption after forward propagation? *)
    if subsume_in_fwd_prop then

      (* Is clause subsumed in frame? *)
      if F.is_subsumed a l then
        
        (

          SMTSolver.trace_comment
            solver
            (Format.asprintf
               "@[<v>Clause is subsumed in frame@,%a@]"
               (pp_print_list Term.pp_print_term "@,")
               (F.values a |> List.map C.actlit_p0_of_clause));
          
          (* Deactivate activation literals of subsumed clause *)
          deactivate_clause solver c';

          (* Increment statistics *)
          Stat.incr Stat.pdr_fwd_subsumed;

          (* Drop clause from frame *)
          a)
          
      else

        (* Subsume in frame with clause *)
        F.subsume a l

    (* Increment statistics *)
    |> count_subsumed solver

    (* Deactivate activation literals of subsumed clauses *)
    |> deactivate_subsumed solver

    (* Continue with frame after subsumption *)
    |> snd
        
    (* Add clause to frame  *)
    |> F.add l c'

    else

      (* Adding a clause may fail if it a prefix of a clause in the
         trie, or if a clause in the trie is a prefix of this
         clause *)
      try F.add l c' a with Invalid_argument _ ->

        (* Is clause subsumed in frame? *)
        if F.is_subsumed a l then
          (deactivate_clause solver c';
           Stat.incr Stat.pdr_fwd_subsumed;
           a)
        else
          F.subsume a l
        |> count_subsumed solver
        |> deactivate_subsumed solver
        |> snd
        |> F.add l c'

  in

  let rec fwd_propagate' solver trans_sys prop frames =

    function 

      (* After the last frame *)
      | [] -> 

        (* Receive and assert new invariants *)
        handle_events solver trans_sys (C.props_of_prop_set prop_set);

        (* Check inductiveness of blocking clauses? *)
        if Flags.pdr_check_inductive () then 

          (

            SMTSolver.trace_comment
              solver
              "fwd_propagate: Checking for inductiveness of clauses \
               in last frame.";

            (* Split clauses to be propagated into the new frame into
               inductive and non-inductive clauses *)
            let non_inductive_clauses, inductive_clauses =
              partition_rel_inductive
                solver
                trans_sys
                []
                []
                prop
            in

            (* Some clauses found inductive *)
            if inductive_clauses <> [] then 

              (

                (* Convert clauses to terms *)
                let inductive_terms =
                  List.map C.term_of_clause inductive_clauses 
                in

                (* Broadcast inductive clauses as invariants *)
                List.iter 
                  (Event.invariant (TransSys.get_scope trans_sys))
                  inductive_terms;

                (* Increment statistics *)
                Stat.incr 
                  ~by:(List.length inductive_clauses) 
                  Stat.pdr_inductive_blocking_clauses;

                (* Add inductive blocking clauses as invariants *)
                List.iter (TransSys.add_invariant trans_sys) inductive_terms;

                SMTSolver.trace_comment
                  solver
                  "fwd_propagate: Asserting new invariants.";

                (* Add invariants to solver instance *)
                List.iter 
                  (function t -> 
                    SMTSolver.assert_term solver t;
                    Term.bump_state Numeral.one t |> SMTSolver.assert_term solver) 
                  inductive_terms

              );

            (* Add a new frame with the non-inductive clauses *)
            let frames' =
              (List.fold_left
                 subsume_and_add
                 F.empty
                 non_inductive_clauses)
              :: frames
            in

            (* DEBUG *)
            if debug_assert then
              assert (check_frames solver prop_set [] frames');

            frames'

          )

        else

          (

            (* Add a new frame with clauses to propagate *)
            let frames' =
              (List.fold_left
                 subsume_and_add
                 F.empty
                 prop)
              :: frames
            in
            
            (* DEBUG *)
            if debug_assert then
              assert (check_frames solver prop_set [] frames');
            
            frames')

      (* Frames in ascending order *)
      | frame :: frames_tl -> 

        (* Receive and assert new invariants *)
        handle_events solver trans_sys (C.props_of_prop_set prop_set);

        SMTSolver.trace_comment
          solver
          (Format.sprintf 
             "fwd_propagate: Checking forward propagation of clauses \
              in frame %d."
             (succ (List.length frames)));

        (* Clauses from frames up to R_k are contained in this and all
           preceding frames up to R_1 *)
        let frames_tl_full = 
          List.fold_left (fun a f -> ((F.values f) @ a)) [] frames_tl
        in

        (* Add propagated clauses to frame with subsumption *)
        let frame' =
          List.fold_left subsume_and_add frame prop
        in
        
        (* DEBUG *)
        if debug_assert then
          assert
            (check_frames'
               solver
               prop_set
               frames_tl_full
               (frame' :: frames));
        
        (* Separate clauses that propagate from clauses to keep in
           this frame *)
        let keep, fwd = 
          partition_fwd_prop
            solver
            trans_sys
            prop_set
            frames_tl_full
            (F.values frame')
        in

        (* Update statistics *)
        Stat.incr 
          ~by:(List.length fwd) 
          Stat.pdr_fwd_propagated;

        (* DEBUG *)
        if debug_assert then
          assert
            (check_frames'
               solver
               prop_set
               (frames_tl_full @ fwd)
               ((List.fold_left
                   (fun a c -> 
                     F.add (C.literals_of_clause c) c a) frame' keep) :: frames));

        (* All clauses propagate? *)
        if keep = [] then 

          (

            Stat.set (List.length frames |> succ) Stat.pdr_fwd_fixpoint;
            
            (* Extract inductive invariant *)
            let ind_inv = 
              (List.fold_left 
                 (fun a c -> List.map C.term_of_clause (F.values c) @ a) 
                 (List.map C.term_of_clause fwd)
                 (frames_tl))
              |> Term.mk_and
            in

            (* Activation literals for inductive invariant *)
            let ind_inv_p0, ind_inv_n0, ind_inv_n1 = 

              let mk = 
                C.create_and_assert_fresh_actlit
                  solver
                  "ind_inv"
                  ind_inv
              in

              mk C.Actlit_p0, mk C.Actlit_n0, mk C.Actlit_n1

            in

            (* DEBUG

               Check if inductive invariant is initial *)
            if debug_assert then
              assert
                (debug_check_sat_assuming
                   solver
                   (function _ -> false)
                   (function _ -> true)
                   [C.actlit_of_frame 0; ind_inv_n0]); 

            (* DEBUG

               Check if inductive invariant is inductive *)
            if debug_assert then
              assert
                (debug_check_sat_assuming
                   solver
                   (function _ -> false)
                   (function _ -> true)
                   [C.actlit_p0_of_prop_set prop_set; ind_inv_p0; ind_inv_n1]); 

            (* Fixpoint found, this frame is equal to the next *)
            raise (Success (List.length frames))

          )

        else

          (

            let fwd' = 

              (* Try propagating clauses before generalization? *)
              if generalize_after_fwd_prop then

                (
                  
                  SMTSolver.trace_comment
                    solver
                    (Format.sprintf 
                       "fwd_propagate: Checking forward propagation of clauses \
                      before generalization in frame %d."
                       (succ (List.length frames)));

                  let copy_parent c =
                    C.parent_of_clause c
                    |> C.literals_of_clause
                    |> C.clause_of_literals solver None
                  in
                  
                  (* Take parent clauses of not propagating clauses and
                     try to propagate *)
                  let _, fwd' = 
                    partition_fwd_prop
                      solver
                      trans_sys
                      prop_set
                      frames_tl_full
                      (List.map copy_parent keep)
                  in
                  
                  (* Update statistics *)
                  Stat.incr ~by:(List.length fwd') Stat.pdr_fwd_gen_propagated;

                  (* Keep clauses as before, in addition propagate
                     non-generalized clauses *)
                  (fwd @ fwd')

                )
                  
              else
                
                (* Propagate clauses as before *)
                fwd 
                  
            in

            (* Propagate clauses in next frame *)
            fwd_propagate' 
              solver
              trans_sys
              fwd'
              ((List.fold_left subsume_and_add F.empty keep)
               :: frames)
              frames_tl

          )

  in

  (* Forward propagate all clauses and add a new frame *)
  fwd_propagate'
    solver
    trans_sys
    []
    []
    (List.rev frames)

             
(*
   TODO: After a restart we want to propagate all used blocking
   clauses into R_1. *)
let rec pdr solver trans_sys prop_set frames =

  (* Must have checked for 0 and 1 step counterexamples, either by
     delegating to BMC or before this point *)
  let bmc_checks_passed prop_set =

    (* Every property is either invariant or at least 1-true *)
    List.for_all 
      (fun (p, _) -> match TransSys.get_prop_status trans_sys p with
        | TransSys.PropInvariant -> true
        | TransSys.PropKTrue k when k >= 1 -> true
        | _ -> false)
      (C.props_of_prop_set prop_set)

  in

  (* Current k is length of trace *)
  let pdr_k = succ (List.length frames) in

  Event.log L_info "PDR main loop at k=%d" pdr_k;

  Event.progress pdr_k;

  Stat.set pdr_k Stat.pdr_k;

  Stat.start_timer Stat.pdr_fwd_prop_time;

  let frames' =

    try 

      (* Forward propagate clauses in all frames *)
      fwd_propagate
        solver
        trans_sys
        prop_set
        frames
        
    (* Fixed point reached *)
    with Success pdr_k -> 

      if 

        (* No 0- or 1-step countexample? *)
        bmc_checks_passed prop_set 
          
      then

        (* Property is proved *)
        raise (Success pdr_k) 

      else

        (* Wait until BMC process has passed k=1 *)
        let rec wait_for_bmc () = 

          Event.log L_info "Waiting for BMC to pass k=1";

          (* Receive messages and update transition system *)
          handle_events solver trans_sys (C.props_of_prop_set prop_set);

          (* No 0- or 1-step countexample? *)
          if bmc_checks_passed prop_set then

            (* Raise exception again *)
            raise (Success pdr_k)

          else

            (

              (* Delay *)
              minisleep 0.1;

              (* Wait *)
              wait_for_bmc ()

            )

        in

        (* Wait until BMC has passed k=1 *)
        wait_for_bmc ()

  in

  Stat.record_time Stat.pdr_fwd_prop_time;

  Stat.set_int_list (frame_sizes frames') Stat.pdr_frame_sizes;

  Stat.start_timer Stat.pdr_strengthen_time;

  (* Recursively block counterexamples in frontier frame *)
  let frames'' = 
    block
      solver
      trans_sys
      prop_set
      ()
      []
      frames' 
  in

  Stat.record_time Stat.pdr_strengthen_time;

  Stat.set_int_list (frame_sizes frames'') Stat.pdr_frame_sizes;

  Stat.update_time Stat.pdr_total_time; 

  (* Output statistics *)
  if output_on_level L_info then print_stats ();

  (* No reachable state violates the property, continue with next k *)
  pdr solver trans_sys prop_set frames''

(* Get a values for the state variables at offset [i], add values to
   path, and return an equational constraint at offset zero for values
   from the model *)
let add_to_path model path state_vars i = 

  (* Turn variable instances to state variables and sort list *)
  let model_i, state_eqs =

    List.fold_left
      (fun (m, eq) sv -> 

         let v = Var.mk_state_var_instance sv i in

         let t = 

           match Var.VarHashtbl.find model v with 
             
             | Model.Term t -> t
               
             | exception Not_found -> 

               TermLib.default_of_type 
                 (StateVar.type_of_state_var sv)
                                         
             | Model.Lambda _ -> assert false
               
         in

         (* Create equation *)
         ((sv, Model.Term t) :: m), 
         Term.mk_eq 
           [Term.mk_var 
              (Var.set_offset_of_state_var_instance Numeral.zero v);
            t]
         :: eq)

      ([], [])
      state_vars

  in
  
  (* Join values of model at current instant to result *)
  let path' = 
    list_join
      StateVar.equal_state_vars
      (List.sort
         (fun (sv1, _) (sv2, _) -> StateVar.compare_state_vars sv1 sv2)
         model_i)
      path
  in

  (* Conjunction of equations to constrain previous state to be equal
     to unprimed state in model *)
  let state = Term.mk_and state_eqs in
  
  (* Return path with state added and constraint for state *)
  (path', state)


(* Extract a concrete counterexample from a sequence of blocking
   clauses *)
let extract_cex_path solver trans_sys trace = 

  SMTSolver.trace_comment
    solver
    "extract_cex_path: extracting concrete counterexample trace.";

  (* Find a state in the head of the sequence of blocking clauses and
     add to the path. Use the activation literal [pre_state] to
     constrain the previous state to the one in the path. *)
  let rec extract_cex_path' path pre_state = function
    
    | [] -> 

      (* Return trace in order *)
      List.map 
        (fun (sv, vl) -> (sv, List.rev vl))
        path
      
    (* Take first blocking clause *)
    | r_i :: tl -> 
      
      (* Find a state in the blocking clause, starting from the given
         state *)
      debug_check_sat_assuming
        solver

        (fun () -> 
           
           (* Add primed state to path, get equational constraint for
              state *)
           let path', state = 
             add_to_path
               (SMTSolver.get_model solver)
               path
               (TransSys.state_vars trans_sys)
               Numeral.one
           in

           (* Activation literal for state *)
           let actlit_p0_state =
             C.create_and_assert_fresh_actlit
               solver
               "cex_path"
               state
               C.Actlit_p0
           in
           
           (* Recurse to continue path out of succeeding blocking
              clause *)
           extract_cex_path' path' actlit_p0_state tl)
        
        (* Counterexample trace must be satisfiable *)
        (fun _ -> assert false)
        
        (* Assume previous state and blocking clause *)
        (pre_state :: C.actlits_n1_of_clause r_i)
        
  in

  (* Start path from initial state into first blocking clause, get
     activation literal for state in R_1 *)
  let init_path, state_init = 
    match trace with 

      (* Must have at least one state *)
      | [] -> assert false

      (* First blocking clause is successor of initial state *)
      | r_1 :: _ -> 

        (* Find an initial state with the first blocking clause as
           successor *)
        debug_check_sat_assuming 
          solver

          (fun () ->

             (* Add unprimed state to empty path, get equational
                constraint for state *)
             add_to_path
               (SMTSolver.get_model solver)
               []
               (TransSys.state_vars trans_sys)
               Numeral.zero)

          (* Counterexample trace must be satisfiable *)
          (fun _ -> assert false)

          (* Assume initial state and blocking clause *)
          ((C.actlit_of_frame 0) ::
           C.actlits_n1_of_clause r_1)

  in

  (* Activation literal for state *)
  let actlit_p0_state_init =
    C.create_and_assert_fresh_actlit
      solver
      "cex_path"
      state_init
      C.Actlit_p0
  in

  (* Extract concrete counterexample starting in a state leading to
     the first blocking clause *)
  extract_cex_path' init_path actlit_p0_state_init trace


(* 

   TODO: Return the maximal R for which P & R & T |= P holds, where R
   contains the blocking clauses before a restart. Therefore we know
   that I |= R. *)
let add_to_r1 clauses = []


(* Helper function for restarts *)
let rec restart_loop trans_sys solver props = 

  (* Exit if no properties left to prove *)
  if props = [] then () else

    (* Properties to prove after restart *)
    let props' = 
      
      try 
        
        (* Reset statistics about frames on restart *)
        Stat.set_int_list [] Stat.pdr_frame_sizes;
        Stat.set_int_list [] Stat.pdr_counterexamples;

        (* Get activation literals for current property set *)
        let prop_set =
          C.prop_set_of_props solver props
        in
        
        (* Run PDR procedure *)
        pdr
          solver 
          trans_sys 
          prop_set
          []

      with 

        (* All propertes are valid *)
        | Success k -> 

          (

            (* Send out valid properties *)
            List.iter
              (fun (p, _) -> 
                 Event.prop_status TransSys.PropInvariant trans_sys p) 
              props;

            (* No more properties remaining *)
            []

          )
          
        (* Some property is invalid *)
        | Counterexample trace -> 

          (

            (* Extract counterexample from sequence of blocking
               clauses *)
            let cex_path =
              extract_cex_path
                solver
                trans_sys
                trace
            in
(*
            debug pdr
                "@[<v>Counterexample:@,@[<hv>%a@]@]"
                (Event.pp_print_path_pt trans_sys false) cex_path
            in
*)
            (* Check which properties are disproved *)
            let props', props_false =

              List.fold_left
                (fun (props', props_false) (p, t) -> 

                   if 

                     (* Property is false along path? *)
                     TransSys.exists_eval_on_path
                       (TransSys.uf_defs trans_sys)
                       ((=) (Eval.ValBool false))
                       t
                       (Model.path_of_list cex_path)

                   then

                     (Event.prop_status 
                        (TransSys.PropFalse cex_path) 
                        trans_sys 
                        p;

                      Event.log
                        L_info 
                        "Property %s disproved by PDR"
                        p;

                      (props', p :: props_false))

                   else

                     (Event.log
                        L_info 
                        "Property %s not disproved by PDR"
                        p;

                      ((p, t) :: props', props_false)))

                ([], [])
                props
            in
(*
            debug pdr
                "Disproved %a, continuing with %a"
                (pp_print_list
                   (fun ppf n -> Format.fprintf ppf "%s" n)
                   "@ ")
                props_false
                (pp_print_list
                   (fun ppf (n, _) -> Format.fprintf ppf "%s" n)
                   "@ ")
                props'
            in
*)
            assert (not (props_false = []));

            props'

          )

        | Disproved prop -> 

          Event.log
            L_info 
            "Some properties are disproved";
          
          (* Check which properties are disproved *)
          let props' =

            List.fold_left
              (fun accum (p, t) -> 

                 (* Property is disproved? *)
                 if TransSys.is_disproved trans_sys p then

                   (Event.log
                     L_info 
                     "Removing disproved property %s"
                     p;
          
                   (* Remove property disproved property from
                        properties to prove *)
                   accum)

                 else 

                   (* Keep property *)
                   (p, t) :: accum)

              []
              props
          in

          props'

        (* Formuala is not in linear integer arithmetic *)
        | Presburger.Not_in_LIA -> 

          (

            Event.log
              L_info
              "Problem contains real valued variables, \
               switching off approximate QE";

            Flags.set_pdr_qe `Z3;

            props

          )

        (* Restart for other reason *)
        | Restart -> props

    in

    if not (props' = []) then 

      (              

        Event.log
          L_info 
          "@[<h>Restarting PDR with properties @[<h>%a@]@]"
          (pp_print_list
             (fun ppf (n, _) -> Format.fprintf ppf "%s" n)
             "@ ")
          props';

        Stat.incr Stat.pdr_restarts);

    (* Restart with remaining properties *)
    restart_loop trans_sys solver props'
    
   
(* Check if the property is valid in the initial state and in the
   successor of the initial state, raise exception [Counterexample] if
   not *)
let rec bmc_checks solver trans_sys props =

  (* Activation literal for frame, is symbol has been declared *)
  let actlit_R0 = C.actlit_of_frame 0 in

  (* Entailment does not hold: split properties in not falsified and
     falsifiable properties *)
  let not_entailed props k () = 

    (* Get model for all variables of transition system *)
    let model =
      (* SMTSolver.get_model solver *)
      SMTSolver.get_var_values
        solver
        (TransSys.vars_of_bounds trans_sys Numeral.zero Numeral.one)
    in

    (* Extract counterexample from solver *)
    let cex =
      Model.path_from_model
        (TransSys.state_vars trans_sys)
        model
        k 
    in

    (* Evaluate term in model *)
    let eval term =
      Eval.eval_term (TransSys.uf_defs trans_sys) model term
      |> Eval.bool_of_value 
    in

    (* Split properties *)
    let not_falsified, falsifiable =
      List.partition
        (fun (_, term) -> Term.bump_state k term |> eval )
        props
    in

    (* Return not falsified and falsifiable properties with
       counterexample *)
    (not_falsified, Some (cex, falsifiable))

  in

  (* Entailment does hold: return all properties as not falsified and
     none as falsifiable *)
  let all_entailed props () = (props, None) in

  (* Check I |= P for given list of properties *)
  let rec bmc_check check_primed = function 

    (* Terminate if all properties falsifiable *)
    | [] -> [] 

    (* Some properties left to check *)
    | props -> 

      (* Create activation literals and assert formulas for property
         set *)
      let prop_set =
        C.prop_set_of_props solver props
      in 

      (* Check satsifiability of I & ~P for all not falsified
         properties P, and partition into not falsified and
         falsifiable *)
      SMTSolver.trace_comment 
        solver
        (Format.sprintf
           "bmc_checks: Check for %s-step counterexample"
           (if check_primed then "one" else "zero"));

      let props', props_falsifiable = 
        debug_check_sat_assuming
          solver
          (not_entailed
             props
             (if check_primed then Numeral.one else Numeral.zero))
          (all_entailed props)
          (if check_primed then
             (actlit_R0 :: 
                (C.actlit_p0_of_prop_set prop_set :: 
                   C.actlits_n1_of_prop_set prop_set))
           else
             (actlit_R0 :: C.actlits_n0_of_prop_set prop_set))
      in

      (* Some properties falsified? *)
      match props_falsifiable with

        (* Some properties falsifiable *)
        | Some (cex, falsifiable) -> 
          
          (* Broadcast properties as falsified with counterexample *)
          List.iter
            (fun (s, _) ->
               Event.prop_status
                 (TransSys.PropFalse (Model.path_to_list cex))
                 trans_sys
                 s)
            falsifiable;
          
          SMTSolver.assert_term
            solver
            (Term.mk_not (C.actlit_p0_of_prop_set prop_set));
            
          (* Check remaining properties *)
          bmc_check check_primed props'

        (* No properties falsifiable *)
        | None -> 

          (* Broadcast properties as 0-true or 1-true *)
          List.iter 
            (fun (s, _) -> 
               Event.prop_status
                 (TransSys.PropKTrue
                    (if check_primed then 1 else 0))
                 trans_sys
                 s)
            props';

          (*
          SMTSolver.assert_term
            solver
            (Term.mk_not (C.actlit_p0_of_prop_set prop_set));
          *)
          
          (* Return properties not falsified *)
          props'

  in

  (* Check if properties hold in the initial state and filter out
     those that don't *)
  let props' = bmc_check false props in

  (* Check if properties hold in the successor of the initial state
     and filter out those that don't *)
  let props'' = bmc_check true props' in

  (* Return 0-true and 1-true properties *)
  props'' 
  

(* Entry point

     If BMC is not running in parallel, check for zero and one step
     counterexamples.

     Run PDR main loop and catch [Success] and [Counterexample]
     exceptions.

*)
let main trans_sys =

  (* PDR solving starts now *)
  Stat.start_timer Stat.pdr_total_time;

  (* Determine logic for the SMT solver *)
  let logic = TransSys.get_logic trans_sys in

  (* Produce unsat cores in SMT solver if flag is set *)
  let produce_cores = Flags.pdr_tighten_to_unsat_core () in

  (* Create new solver instance *)
  let solver = 
    SMTSolver.create_instance
      ~produce_assignments:true
      ~produce_cores:produce_cores
      logic
      (Flags.smtsolver ())
  in

  (* Save solver instance for clean exit *)
  ref_solver := Some solver;

  (* Declare uninterpreted function symbols *)
  SMTSolver.trace_comment 
    solver
    "main: Declare state variables and define predicates";

  (* Declare uninterpreted function symbols *)
  TransSys.init_define_fun_declare_vars_of_bounds
    trans_sys
    (SMTSolver.define_fun solver)
    (SMTSolver.declare_fun solver)
    Numeral.(~- one) Numeral.one;

  (* Get invariants of transition system *)
  let invars_1 = 
    TransSys.invars_of_bound trans_sys Numeral.one 
  in

  (* Get invariants for current state *)
  let invars_0 = 
    TransSys.invars_of_bound
      trans_sys
       ~one_state_only:true 
       Numeral.zero 
  in

  (* Assert invariants for current state if not empty *)
  if not (invars_0 == Term.t_true) then 

    (SMTSolver.trace_comment solver "main: Assert invariants";
     SMTSolver.assert_term solver invars_0;
     SMTSolver.assert_term solver invars_1);

  (* Create activation literal for frame R_0 *)
  let actconst_r0, actlit_r0 =
    C.actlit_symbol_of_frame 0, C.actlit_of_frame 0
  in 

  (* Declare symbol in solver *)
  SMTSolver.declare_fun solver actconst_r0;

  Stat.incr Stat.pdr_activation_literals;
  
  (* Assert initial state constraint guarded with activation literal

     a_R0 => I[x] *)
  SMTSolver.trace_comment solver "main: Assert guarded initial state";
  SMTSolver.assert_term 
    solver
    (Term.mk_implies
       [actlit_r0;
        (TransSys.init_of_bound trans_sys Numeral.zero)]);

  (* Assert transition relation unguarded

     T[x,x'] *)
  SMTSolver.trace_comment solver "main: Assert unguarded transition relation"; 
  SMTSolver.assert_term 
    solver
    (TransSys.trans_of_bound trans_sys Numeral.one);

  (* Print inductive assertions to file? *)
  (match Flags.pdr_print_to_file () with 

    (* Keep default formatter *)
    | None -> ()

    (* Output to given file *)
    | Some f -> 

      (* Output channel on file *)
      let oc = 
        try open_out f with
          | Sys_error _ -> 
            failwith "Could not open file for inductive assertions"
      in 

      (* Create formatter and store in reference *)
      ppf_inductive_assertions := Format.formatter_of_out_channel oc);

  (* Properties to prove from the transition system *)
  let trans_sys_props = 
    TransSys.props_list_of_bound trans_sys Numeral.zero 
  in

  (* Check for zero and one step counterexamples and continue with
     remaining properties *)
  let props' =

    (* Is BMC running in parallel? *)
    if List.mem `BMC (Flags.enable ()) then 

      (Event.log L_info
         "Delegating check for zero and one step counterexamples \
          to BMC process.";

       trans_sys_props)

    else

      (* BMC is not running, must check here *)
      bmc_checks
        solver
        trans_sys
        trans_sys_props

  in

  (* Run and restart on disproved properties *)
  restart_loop trans_sys solver props'


(* 
   Local Variables:
   compile-command: "make -C .. -k"
   tuareg-interactive-program: "./kind2.top -I ./_build -I ./_build/SExpr"
   indent-tabs-mode: nil
   End:
*)
