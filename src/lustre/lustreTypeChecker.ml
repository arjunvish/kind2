(* This file is part of the Kind 2 model checker.

   Copyright (c) 2020 by the Board of Trustees of the University of Iowa

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

(* @author Apoorv Ingle *)

(* TODO: Find strongly connected components, put them in a group *)
(* TODO: This also checks if we have co-dependent types or values *)
(* TODO: UserType if they are alias of any other type, then flatten them out *)
(* TODO: Introduce GroupType which is like tuple type but has flattened cannonical structure*)

module R = Res

module LA = LustreAst
module LC = LustreContext
module LH = LustreAstHelpers

(** Returns [ok] if the typecheck/typeinference runs fine 
 * or reports an error at position with the error *)
type 'a tcResult = ('a, Lib.position * string) result

(** Type alias for lustre type from LustreAst  *)
type tcType  = LA.lustre_type

(** String of the type to display in type errors *)
let string_of_tcType: tcType -> string = fun t -> Lib.string_of_t LA.pp_print_lustre_type t
                       
(** Map for types with identifiers as keys *)
module IMap = struct
  (** everything that [Stdlib.Map] gives us  *)
  include Map.Make(struct
              type t = LA.ident
              let compare i1 i2 = Stdlib.compare i1 i2
            end)
  (** Pretty print type synonyms*)
  let pp_print_type_binding ppf = fun i ty -> 
    Format.fprintf ppf "(%a<->%a), " LA.pp_print_ident i LA.pp_print_lustre_type ty

  (** Pretty print type bindings*)
  let pp_print_type_binding ppf = fun i ty -> 
    Format.fprintf ppf "(%a:%a), " LA.pp_print_ident i LA.pp_print_lustre_type ty

  (** Pretty print value bindings (used for constants)*)
  let pp_print_val_binding ppf = fun i (v, ty) ->
    Format.fprintf ppf "(%a:%a :-> %a), " LA.pp_print_ident i LA.pp_print_lustre_type ty LA.pp_print_expr v

  (** Pretty print type context *)
  let pp_print_tySyns ppf = iter (pp_print_type_binding ppf)   

  (** Pretty print type context *)
  let pp_print_tymap ppf = iter (pp_print_type_binding ppf)   

  (** Pretty print value store *)
  let pp_print_vstore ppf = iter (pp_print_val_binding ppf)
end

let sortTypedIdent: LA.typed_ident list -> LA.typed_ident list = fun tyIdents ->
  List.sort (fun (_,i1,_) (_,i2,_) -> Stdlib.compare i1 i2) tyIdents

let sortIdents: LA.ident list -> LA.ident list = fun ids ->
  List.sort (fun i1 i2 -> Stdlib.compare i1 i2) ids

type tyAliasStore = tcType IMap.t
(** A store of type Aliases, i.e. for user defined types  *)

type tyStore = tcType IMap.t
(** A store of identifier and their types*)

type constStore = (LA.expr * tcType) IMap.t 
(** A Store of constant identifier and their (const) values with types. 
 *  The values of the associated identifiers should be evaluated to a 
 * Bool or an Int at constant propogation phase of type checking *)
            
(** Type Checker context is a pair type store and a value store with identifier as its key *)
type tcContext = { tySyns: tyAliasStore; tyCtx:tyStore; vlCtx: constStore}

let emptyContext: tcContext = {tySyns = IMap.empty; tyCtx= IMap.empty; vlCtx=IMap.empty}

(** Pretty print the complete type checker context*)
let pp_print_tcContext ppf ctx
  = Format.fprintf ppf
      ("TypeSynonyms={%a}\n"
       ^^ "TypeContext={%a}\n"
       ^^ "ConstValueContext={%a}")
      IMap.pp_print_tySyns (ctx.tySyns)
      IMap.pp_print_tymap (ctx.tyCtx)
      IMap.pp_print_vstore (ctx.vlCtx)

(**********************************************
 * Helper functions for type checker workings *
 **********************************************)
  
(** [typeError] returns an [Error] of [tcResult] *)
let typeError pos err = R.error (pos, "Type error: " ^ err)
                      
let memberTySyn: tcContext -> LA.ident -> bool
  = fun ctx i -> IMap.mem i (ctx.tySyns)
           
let memberTy: tcContext -> LA.ident -> bool
  = fun ctx i -> IMap.mem i (ctx.tyCtx)

let memberVal: tcContext -> LA.ident -> bool
  = fun ctx i -> IMap.mem i (ctx.vlCtx)

                      
let lookupTySyn: tcContext -> LA.ident -> tcType =
  fun ctx i -> IMap.find i (ctx.tySyns)
                      
let lookupTy: tcContext -> LA.ident -> tcType =
  fun ctx i -> let ty = IMap.find i (ctx.tyCtx) in
               match ty with
               | LA.UserType (_, uid) ->
                  if (memberTySyn ctx uid)
                  then (lookupTySyn ctx uid)
                  else ty
               | _ ->  ty

let lookupConst: tcContext -> LA.ident -> (LA.expr * tcType) =
  fun ctx i -> IMap.find i (ctx.vlCtx)

let addTySyn: tcContext -> LA.ident -> tcType -> tcContext = 
  fun ctx i ty -> {ctx with tySyns=IMap.add i ty (ctx.tySyns)}

let addTy: tcContext -> LA.ident -> tcType -> tcContext
  = fun ctx i ty -> {ctx with tyCtx=IMap.add i ty (ctx.tyCtx)}

let addConst: tcContext -> LA.ident -> LA.expr -> tcType -> tcContext
  = fun ctx i e ty -> {ctx with vlCtx=IMap.add i (e, ty) ctx.vlCtx} 

let union: tcContext -> tcContext -> tcContext
  = fun ctx1 ctx2 -> { tySyns = (IMap.union (fun k v1 v2 -> Some v2) (ctx1.tySyns) (ctx2.tySyns))
                     ; tyCtx = (IMap.union (fun k v1 v2 -> Some v2) (ctx1.tyCtx) (ctx2.tyCtx))
                     ; vlCtx = (IMap.union (fun k v1 v2 -> Some v2) (ctx1.vlCtx) (ctx2.vlCtx))
                     }

let singletonTy: LA.ident -> tcType -> tcContext =
  fun i ty -> addTy emptyContext i ty

let singletonConst: LA.ident -> LA.expr -> tcType -> tcContext =
  fun i e ty -> addConst emptyContext i e ty

let extractIpTy: LA.const_clocked_typed_decl -> LA.ident * tcType
  = fun  (_, i, ty, _, _) -> (i, ty)

let extractOpTy: LA.clocked_typed_decl -> LA.ident * tcType
  = fun (_, i, ty, _) -> (i, ty)
                           
let isConstArg: LA.const_clocked_typed_decl -> bool
  = fun (_, _, _, _, isConst) -> isConst

let extractArgCtx: LA.const_clocked_typed_decl -> tcContext
  = fun input -> let (i, ty) = extractIpTy input in
                 singletonTy i ty

let extractRetCtx: LA.clocked_typed_decl -> tcContext
  = fun op -> let (i, ty) = extractOpTy op in
              singletonTy i ty
                       
let extractConsts: LA.const_clocked_typed_decl -> tcContext
  = fun (pos, i, ty, _, isConst) ->
  if isConst
  then singletonConst i (LA.Ident (pos, i)) ty
  else emptyContext 
  
(**********************************************
 * Type inferring and type checking functions *
 **********************************************)
              
let inferTypeConst: Lib.position -> LA.constant -> tcType
  = fun pos -> function
  | Num _ -> Int pos
  | Dec _ -> Real pos
  | _ -> Bool pos

       
let inferTypeUnaryOp: Lib.position -> LA.unary_operator -> tcType tcResult =
  fun pos -> function
  | LA.Not -> R.ok (LA.TArr (pos ,Bool pos , Bool pos))
  | LA.BVNot
  | LA.Uminus -> R.ok (LA.TArr (pos, Int pos, Int pos))

(** TODO: There is some polymorphism going on here due to overloaded Plus/Times/Minus operations *)
let inferTypeBinaryOp: Lib.position -> LA.binary_operator -> tcType tcResult = fun pos ->
  function
  | LA.And | LA.Or | LA.Xor | LA.Impl
    -> R.ok (LA.TArr (pos, Bool pos, TArr(pos, Bool pos, Bool pos)))
  | LA.Mod | LA. Minus | LA.Plus | LA.Times | LA.IntDiv
    | LA.BVAnd | LA.BVOr | LA.BVShiftL | LA.BVShiftR
    -> R.ok (LA.TArr (pos, Int pos, TArr(pos, Int pos, Int pos)))
  | LA.Div -> R.ok (LA.TArr (pos, Real pos, TArr(pos, Real pos, Real pos)))
     
let inferTypeConvOp: Lib.position -> tcType -> LA.conversion_operator -> tcType tcResult = fun pos ty ->
  function
  | ToInt -> R.ok (LA.TArr (pos, ty, Int pos))
  | ToReal -> R.ok (LA.TArr (pos, ty, Real pos))
  | ToInt8 -> R.ok (LA.TArr (pos, ty, Int8 pos))
  | ToInt16 -> R.ok (LA.TArr (pos, ty, Int16 pos))
  | ToInt32 -> R.ok (LA.TArr (pos, ty, Int32 pos))
  | ToInt64 -> R.ok (LA.TArr (pos, ty, Int64 pos))
  | ToUInt8 -> R.ok (LA.TArr (pos, ty, UInt8 pos))
  | ToUInt16 -> R.ok (LA.TArr (pos, ty, UInt16 pos))
  | ToUInt32 -> R.ok (LA.TArr (pos, ty, UInt32 pos))
  | ToUInt64 -> R.ok (LA.TArr (pos, ty, UInt64 pos))

let inferTypeCompOp: Lib.position -> tcType -> LA.comparison_operator -> tcType tcResult = fun pos ty ->
  function
  |_ -> R.ok (LA.TArr (pos, ty, LA.TArr (pos, ty, Bool pos))) 

let inferTypeGrpExpr: LA.expr -> tcType = function | _ -> Lib.todo __LOC__
                                                        
(** Infer the type of a [LA.expr] with the types of free variables given in [tcContext] *)
let rec inferTypeExpr: tcContext -> LA.expr -> tcType tcResult
  = fun ctx -> function
  (* Identifiers *)
  | LA.Ident (pos, i) ->
     (try R.ok (lookupTy ctx i) with
      | Not_found -> typeError pos ("Unbound Variable: " ^ i)) 
  | LA.ModeRef (pos, is) ->      
     let lookupModeTy ctx i =
       (try R.ok (lookupTy ctx i) with
        | Not_found -> typeError pos ("Unbound Variable: " ^ i)) in
     R.bind(R.seq (List.map (lookupModeTy ctx) is)) (fun tys ->
         if List.length tys = 1
         then R.ok (List.hd tys)
         else R.ok (LA.TupleType (pos, tys)))
  | LA.RecordProject (pos, expr, fld) ->
     R.bind (inferTypeExpr ctx expr) (fun recTy ->
         match recTy with
         | LA.RecordType (_, flds) ->
            let typedFields = List.map (fun (_, i, ty) -> (i, ty)) flds in
            (match (List.assoc_opt fld typedFields) with
             | Some ty -> R.ok ty
             | None -> typeError pos ("No field named " ^ fld ^ "found in record type")) 
         | _ -> typeError pos ("Cannot project field out of non record expression type "
                               ^ string_of_tcType recTy))
  | LA.TupleProject (pos, e1, e2) -> Lib.todo __LOC__ 

  (* Values *)
  | LA.Const (pos, c) -> R.ok (inferTypeConst pos c)

  (* Operator applications *)
  | LA.UnaryOp (pos, op, e) ->
     R.bind (inferTypeUnaryOp pos op) (fun ty ->
         match ty with
         | TArr (_,argTy, resTy) ->
            R.bind (checkTypeExpr ctx e argTy) (fun _ ->
                R.ok resTy)
         | fty -> typeError pos ("Unexpected unary operator type: "
                                 ^ string_of_tcType fty))
  | LA.BinaryOp (pos, bop, e1, e2) ->
     R.bind (inferTypeBinaryOp pos bop) (fun ty ->
         match ty with
         | TArr (_,argTy1, TArr (_,argTy2, resTy)) ->
            (R.bind (checkTypeExpr ctx e1 argTy1) (fun _ ->
                 R.bind (checkTypeExpr ctx e2 argTy2) (fun _ ->
                     R.ok resTy)))
         | fty -> typeError pos ("Unexpected binary operator type: "
                                 ^ string_of_tcType fty))
  | LA.TernaryOp (pos, top, con, e1, e2) ->
     R.bind (inferTypeExpr ctx con) (fun cTy ->
         match cTy with
         | Bool _ ->  R.bind (inferTypeExpr ctx e1) (fun e1Ty->
                          R.bind (checkTypeExpr ctx e1 e1Ty) (fun _ ->
                              R.ok e1Ty))
         | _   ->  typeError pos ("Expected a boolean expression but found "
                                  ^ string_of_tcType cTy))
  | LA.NArityOp _ -> Lib.todo __LOC__          (* One hot expression is not supported *)    
  | LA.ConvOp (pos, cop, e) ->
     R.bind (inferTypeExpr ctx e)(fun eTy -> 
     R.bind (inferTypeConvOp pos eTy cop) (fun ty ->
         match ty with
         | TArr (_,argTy, resTy) -> R.ok resTy
         | _ -> typeError pos ("Unexpected conversion operator type: "
                                 ^ string_of_tcType ty)))
  | LA.CompOp (pos, cop, e1, e2) ->
     R.bind (inferTypeExpr ctx e1) (fun ty1 -> 
         R.bind (inferTypeCompOp pos ty1 cop) (fun cty ->
             match cty with
             | TArr (_,_, TArr (_,argTy2, resTy)) ->
                R.bind (checkTypeExpr ctx e2 argTy2) (fun _ -> 
                    R.ok resTy)
             | fty -> typeError pos ("Unexpected comparison operator type: "
                                     ^ string_of_tcType fty)))
    
  (* Structured expressions *)
  | LA.RecordExpr (pos, _, flds) ->
     let inferField: tcContext -> (LA.ident * LA.expr) -> (LA.typed_ident) tcResult
       = fun ctx (i, e) ->
       R.bind (inferTypeExpr ctx e) (fun ty ->
           R.ok (LH.pos_of_expr e, i, ty))       
     in R.bind (R.seq (List.map (inferField ctx) flds)) (fun fldTys -> 
            R.ok (LA.RecordType (pos, fldTys)))    
  | LA.GroupExpr (pos, structType, exprs)  ->
     (match structType with
       | LA.ExprList 
         | LA.TupleExpr ->
          R.bind (R.seq (List.map (inferTypeExpr ctx) exprs)) (fun tys ->
              R.ok (LA.TupleType (pos, tys)))
       | LA.ArrayExpr ->
          R.bind (R.seq (List.map (inferTypeExpr ctx) exprs)) (fun tys ->
              let elty = List.hd tys in
              R.bind (R.seqM (&&) true (List.map (eqLustreType ctx elty) tys)) (fun isEq ->
                  if isEq
                  then let arrTy = List.hd tys in
                       let arrSize = LA.Const (pos, Num (string_of_int (List.length tys))) in
                       R.ok (LA.ArrayType (pos, (arrTy, arrSize)))
                  else typeError pos "All expressions must be of the same type in an Array")))
    
  (* Update structured expressions *)
  | LA.ArrayConstr (pos, bExpr, supExpr) ->
     R.bind (inferTypeExpr ctx bExpr) (fun bTy ->
         R.bind (inferTypeExpr ctx supExpr) (fun supTy ->
                 if isExprIntType ctx supExpr
                 then R.ok(LA.ArrayType (pos, (bTy, supExpr)))
                 else typeError pos "Array cannot have non numeral type as its bounds"))
  | LA.StructUpdate _ -> Lib.todo __LOC__
  | LA.ArraySlice (pos, e1, (il, iu)) ->
     if isExprIntType ctx il && isExprIntType ctx iu
     then R.bind (inferTypeExpr ctx e1) (fun ty ->
               match ty with
               | ArrayType (_, (bTy, s)) -> R.ok (LA.ArrayType (pos, (bTy, LH.abs_diff pos il iu)))
               | _ -> typeError pos ("Slicing can only be done on an type Array "
                                       ^ "but found type "
                                       ^ Lib.string_of_t LA.pp_print_lustre_type ty))
     else typeError pos ("Slicing should have integer types")
  | LA.ArrayIndex (pos, e, i) ->
     if isExprIntType ctx i
     then R.bind (inferTypeExpr ctx e) (fun ty ->
              match ty with
              | ArrayType (_, (bTy, _)) -> R.ok bTy
              | _ -> typeError pos ("Indexing can only be done on an type Array "
                                    ^ "but found type "
                                    ^ Lib.string_of_t LA.pp_print_lustre_type ty))
     else typeError pos ("Array Index should have integer types") 
  | LA.ArrayConcat (pos, arr1, arr2) ->
     R.bind (inferTypeExpr ctx arr1)(fun ty1 ->
         match ty1 with
         | ArrayType (_, (bTy1, size1)) ->
            R.bind(inferTypeExpr ctx arr2)(fun ty2 ->
                match ty2 with
                | ArrayType (_, (bTy2, size2)) ->
                   R.bind(eqLustreType ctx bTy1 bTy2) (fun isEq ->
                       if isEq && (isExprIntType ctx size1) && (isExprIntType ctx size2) 
                       then R.ok (LA.ArrayType (pos, (bTy1, LH.addExp pos size1 size2)))
                       else typeError pos ("Cannot concat array of two different types "
                                           ^ string_of_tcType ty1 ^ " and "
                                           ^ string_of_tcType ty2 ))
                | _ -> typeError pos ("Cannot concat non-array type "
                                      ^ string_of_tcType ty2)) 
         | _ -> typeError pos ("Cannot concat non-array type "
                               ^ string_of_tcType ty1))

  (* Quantified expressions *)
  | LA.Quantifier (pos, _, qs, e) ->
          let extnCtx = List.fold_left union ctx
                          (List.map (fun (_, i, ty) -> singletonTy i ty) qs) in
          inferTypeExpr extnCtx e 

  (* Clock operators *)
  | LA.When (_, e, _) -> inferTypeExpr ctx e
  | LA.Current (_, e) -> inferTypeExpr ctx e
  | LA.Condact (pos, c, _, node, args, defaults) ->
     R.bind (checkTypeExpr ctx c (Bool pos)) (fun _ ->
         R.bind (inferTypeExpr ctx (Call (pos, node, args))) (fun rTy ->
             R.bind (R.seq (List.map (inferTypeExpr ctx) defaults)) (fun dTys -> 
                 R.bind (eqLustreType ctx rTy (TupleType (pos, dTys)))(fun isEq -> 
                 if isEq then R.ok rTy
                 else typeError pos "Defaults do not have the same type as node call"))))
  | LA.Activate (pos, node, cond, rcond, args) ->
     R.bind (checkTypeExpr ctx cond (Bool pos)) (fun _ ->
         inferTypeExpr ctx (Call (pos, node, args)))
  | LA.Merge (pos, i, mcases) ->
     R.bind (inferTypeExpr ctx (LA.Ident (pos, i))) (fun _ ->
         let caseTys = List.map snd mcases |> List.map (inferTypeExpr ctx) in
         R.bind(R.seq caseTys) (fun tys ->
             let mainTy = List.hd tys in
             R.bind(R.seq (List.map (eqLustreType ctx mainTy) tys)) (fun isEqs -> 
                 if
                   List.fold_left (&&) true isEqs
                 then R.ok mainTy
                 else typeError pos ("All expressions in merge expected to be the same type "
                                     ^ string_of_tcType mainTy))))
  | LA.RestartEvery (pos, node, args, cond) ->
     R.bind(checkTypeExpr ctx cond (LA.Bool pos)) (fun _ ->
         inferTypeExpr ctx (LA.Call (pos, node, args)))
                                
  (* Temporal operators *)
  | LA.Pre (pos, e) -> inferTypeExpr ctx e
  | LA.Last (pos, i) -> inferTypeExpr ctx (LA.Ident (pos, i))
  | LA.Fby (pos, e1, _, e2) ->
     R.bind (inferTypeExpr ctx e1) (fun ty1 ->
         R.bind(inferTypeExpr ctx e2)(fun ty2 ->
             R.bind(eqLustreType ctx ty1 ty2)(fun isEq ->
                 if isEq
                 then R.ok ty1
                 else typeError pos ("Both the expressions in Fby should be of the same type."
                                     ^ "Found types " ^ string_of_tcType ty1
                                     ^ " and " ^ string_of_tcType ty2))))
  | LA.Arrow (pos, e1, e2) ->
     R.bind (inferTypeExpr ctx e1) (fun ty1 ->
         R.bind (inferTypeExpr ctx e2) (fun ty2 ->
             R.bind((eqLustreType ctx ty1 ty2)) (fun isEq -> 
                 if isEq then R.ok ty1 else
                   typeError pos
                     ("Arrow types do not match "
                      ^ string_of_tcType ty1
                      ^ " and " ^ string_of_tcType ty2)))) 

  (* Node calls *)
  | LA.Call (pos, i, argExprs) ->
     Log.log L_debug "Inferring type for node call %a" LA.pp_print_ident i  
    ; let rec inferTypeNodeArgs: tcContext -> LA.expr list -> tcType tcResult
        = fun ctx args ->
        R.bind (R.seq (List.map (inferTypeExpr ctx) args)) (fun argTys ->
            R.ok (LA.TupleType (pos, argTys)))
      in
      (match (lookupTy ctx i) with 
       | TArr (_, expArgTys, expRetTys) ->
          R.bind (inferTypeNodeArgs ctx argExprs) (fun givenArgTys ->
              R.bind (eqLustreType ctx expArgTys givenArgTys) (fun isArgTyOk ->
                  if isArgTyOk
                  then R.ok expRetTys
                  else typeError pos ("Node arguments expected to have type "
                                      ^ string_of_tcType expArgTys
                                      ^ " but found type "
                                      ^ string_of_tcType givenArgTys)))
       | ty -> typeError pos
                 ("Expected node type to be a function type, but found type"
                  ^ string_of_tcType ty)) 
      
  | LA.CallParam _ -> Lib.todo __LOC__

(** Type checks an expression and returns [ok] 
 * if the expected type is the given type [tcType]  
 * returns an [Error of string] otherwise *)
and checkTypeExpr: tcContext -> LA.expr -> tcType -> unit tcResult
  = fun ctx expr expTy ->
  match expr with
  (* Identifiers *)
  | Ident (pos, i) as ident ->
     R.bind (inferTypeExpr ctx ident) (fun ty ->
         R.bind (eqLustreType ctx ty expTy) (fun isEq ->
             if isEq
             then R.ok ()
             else typeError pos ("Variable " ^ i
                                 ^ " does not match expected type "
                                 ^ string_of_tcType expTy
                                 ^ " with infered type "
                                 ^ string_of_tcType ty)))

  | ModeRef (pos, ids) -> Lib.todo __LOC__
  | RecordProject (pos, expr, fld) -> checkTypeRecordProj pos ctx expr fld expTy
  | TupleProject (pos, e1, e2) -> Lib.todo __LOC__ 

  (* Operators *)
  | UnaryOp (pos, op, e) ->
     R.bind (inferTypeUnaryOp pos op) (fun ty ->
         R.bind (inferTypeExpr ctx e) (fun argTy ->
             R.bind (eqLustreType ctx ty (TArr(pos, argTy, expTy))) (fun isEq ->
                 if isEq
                 then R.ok ()
                 else typeError pos ("Cannot apply argument of type "
                                     ^ string_of_tcType argTy
                                     ^ " to operator of type "
                                     ^ string_of_tcType ty))))
  | BinaryOp (pos, op, e1, e2) ->
     R.bind (inferTypeBinaryOp pos op) (fun ty ->
         R.bind (inferTypeExpr ctx e1) (fun argTy1 ->
             R.bind (inferTypeExpr ctx e2) (fun argTy2 ->
                 R.bind (eqLustreType ctx ty (TArr (pos, argTy1, TArr (pos, argTy2, expTy)))) (fun isEq ->
                     if isEq 
                     then R.ok ()
                     else typeError pos (" Cannot apply arguments of type "
                                         ^ string_of_tcType argTy1
                                         ^ " and type " ^ string_of_tcType argTy2
                                         ^ " to obtain type " ^ string_of_tcType expTy
                                         ^ " to operator of type " ^ string_of_tcType ty)))))
  | LA.TernaryOp (pos, op, con, e1, e2) ->
     R.bind (inferTypeExpr ctx con) (fun ty ->
         match ty with
         | Bool _ -> R.bind (inferTypeExpr ctx e1) (fun ty1 ->
                         R.bind (inferTypeExpr ctx e2) (fun ty2 ->
                             R.bind (eqLustreType ctx ty1 ty2)(fun isEq ->
                                 if isEq
                                 then R.ok ()
                                 else typeError pos
                                        ("Cannot unify type "
                                         ^ string_of_tcType ty1
                                         ^ "with type "
                                         ^ string_of_tcType ty2))))
         | _ -> typeError pos ("ITE condition expression "
                               ^ "Expected: " ^ string_of_tcType (Bool pos)
                               ^ "Found: " ^ string_of_tcType ty))
  | NArityOp _ -> Lib.todo __LOC__          (* One hot expression is not supported? *)    
  | ConvOp (pos, cvop, e) ->
     R.bind (inferTypeExpr ctx e) (fun eTy ->
         R.bind (inferTypeConvOp pos eTy cvop) (fun cvopTy ->
             match cvopTy with
             | TArr (pos, _, retTy) ->
                R.bind (eqLustreType ctx cvopTy expTy)(fun isEq ->
                    if isEq
                    then R.ok ()
                    else typeError pos ("Expected type " ^ string_of_tcType expTy
                                        ^ " but found type " ^ string_of_tcType retTy))
             | _ -> typeError pos ("Cannot apply argument of type "
                                        ^ string_of_tcType eTy
                                        ^ " to operator of type "
                                        ^ string_of_tcType cvopTy)))
  | CompOp (pos, cop, e1, e2) ->
     R.bind (inferTypeExpr ctx e1) (fun argTy1 ->
         R.bind (inferTypeExpr ctx e2) (fun argTy2 ->
             R.bind (eqLustreType ctx argTy1 argTy2) (fun isEq ->
                     if isEq 
                     then R.ok ()
                     else typeError pos
                            ("Cannot compare values of different types "
                             ^ string_of_tcType argTy1
                             ^ " and " ^ string_of_tcType argTy2))))

  (* Values/Constants *)
  | Const (pos, c) -> R.ok ()

  (* Structured expressions *)
  | RecordExpr (pos, _, flds) ->
     let (ids, es) = List.split flds in
     let mkTyIdent p i t = (p, i, t) in    
     R.bind (R.seq (List.map (inferTypeExpr ctx) es)) (fun infTys ->
         let infRTy = LA.RecordType (pos, (List.map2 (mkTyIdent pos) ids infTys)) in
         R.bind (eqLustreType ctx expTy infRTy) (fun isEq ->
             if isEq then R.ok ()
             else typeError pos
                    ("RecordType mismatch expected "
                     ^ string_of_tcType expTy
                     ^ " but found "
                     ^ string_of_tcType infRTy)))
  | GroupExpr (pos, groupTy, es) ->
     (match groupTy with
      (* These should be tuple type  *)
      | ExprList
        | TupleExpr ->
         (match expTy with
          | TupleType (_, tys) ->
             if List.length tys != List.length es
             then typeError pos "Size of tuple does not match size of expression list"
             else R.seq_ (List.map2 (checkTypeExpr ctx) es tys)
          | _ -> typeError pos ("Expression list and Tuple list "
                                ^ "should be of type Tuple but found "
                                ^ string_of_tcType expTy))
      (* This should be array type *)
      | ArrayExpr ->
         (match expTy with
          | ArrayType (_, (bTy, _)) ->
             R.seq_ (Lib.list_apply (List.map (checkTypeExpr ctx) es) bTy)
          | _ -> typeError pos ("Array type expected but found "
                 ^ string_of_tcType expTy))
     )

  (* Update of structured expressions *)
  | StructUpdate _ -> Lib.todo __LOC__
  | ArrayConstr (pos, bExp, supExp) ->
     R.bind (inferTypeExpr ctx bExp) (fun bTy ->
         R.bind (inferTypeExpr ctx supExp) (fun supTy ->
             R.bind (eqLustreType ctx expTy (LA.ArrayType (pos, (bTy, bExp)))) (fun isEq ->
                 if isEq
                 then R.ok()
                 else typeError pos ("Expecting array type "
                                     ^ string_of_tcType expTy
                                     ^ " but found "
                                     ^ string_of_tcType (LA.ArrayType (pos, (bTy, bExp)))))))
  | ArrayIndex (pos, e, idx) -> 
     if isExprIntType ctx idx
     then checkTypeExpr ctx e (ArrayType (pos, (expTy, (LA.Const (pos, Num "10")))))
     else typeError pos ("Array index should have an integer type")

  | ArraySlice _ -> Lib.todo __LOC__
  | ArrayConcat _ -> Lib.todo __LOC__

  (* Quantified expressions *)
  | Quantifier (pos, _, qs, e) ->
     let extnCtx = List.fold_left union ctx
                     (List.map (fun (_, i, ty) -> singletonTy i ty) qs) in
     checkTypeExpr extnCtx e expTy

  (* Clock operators *)
  | When (_, e, _) -> checkTypeExpr ctx e expTy
  | Current (_, e) -> checkTypeExpr ctx e expTy
  | Condact (pos, c, _, node, args, defaults) ->
     R.bind (checkTypeExpr ctx c (Bool pos)) (fun _ ->
         R.bind (checkTypeExpr ctx (Call (pos, node, args)) expTy) (fun _ ->
             R.bind (R.seq (List.map (inferTypeExpr ctx) defaults)) (fun dTys -> 
                 R.bind (eqLustreType ctx expTy (TupleType (pos, dTys)))(fun isEq -> 
                     if isEq then R.ok ()
                     else typeError pos "Defaults do not have the same type as node call"))))
  | Activate (pos, node, cond, rcond, args) -> 
     R.bind (checkTypeExpr ctx cond (Bool pos)) (fun _ ->
         checkTypeExpr ctx (Call (pos, node, args)) expTy) 
  | Merge (pos, i, mcases) ->
     R.bind (inferTypeExpr ctx (LA.Ident (pos, i))) (fun _ ->
         let checkedTys = List.map snd mcases |> List.map (fun e -> checkTypeExpr ctx e expTy) in
         R.bind(R.seq checkedTys) (fun _ -> R.ok ()))
  | RestartEvery (pos, node, args, cond) ->
     R.bind(checkTypeExpr ctx cond (LA.Bool pos)) (fun _ ->
         checkTypeExpr ctx (LA.Call (pos, node, args)) expTy)

  (* Temporal operators *)
  | Pre (pos, e) -> checkTypeExpr ctx e expTy
  | Last (pos, i) ->
     R.bind (inferTypeExpr ctx (LA.Ident (pos, i))) (fun ty ->
         R.bind (eqLustreType ctx ty expTy) (fun isEq ->
             if isEq
             then R.ok ()
             else typeError pos ("Indentifier " ^ i
                                 ^ " does not match expected type "
                                 ^ string_of_tcType expTy
                                 ^ " with infered type "
                                 ^ string_of_tcType ty)))
  | Fby (pos, e1, _, e2) ->
     R.bind (checkTypeExpr ctx e1 expTy) (fun _ ->
         R.bind (checkTypeExpr ctx e2 expTy) (fun _ ->
             R.ok ()))
  | Arrow (pos, e1, e2) ->
     R.bind(inferTypeExpr ctx e1) (fun ty1 ->
         R.bind(inferTypeExpr ctx e2) (fun ty2 ->
             R.bind (eqLustreType ctx ty1 ty2)(fun isEq ->
                 if isEq 
                 then R.ok ()
                 else typeError pos (" Cannot match expected type "
                                     ^ string_of_tcType ty1
                                     ^ " with " ^ string_of_tcType ty2))))

  (* Node calls *)
  | Call (pos, i, args) ->
     R.bind (R.seq (List.map (inferTypeExpr ctx) args)) (fun argTys ->
         checkTypeExpr ctx (LA.Ident (pos, i)) (TArr (pos, TupleType (pos, argTys), expTy)))
  | CallParam _ -> Lib.todo __LOC__

and checkTypeRecordProj: Lib.position -> tcContext -> LA.expr -> LA.index -> tcType -> unit tcResult =
  fun pos ctx expr idx expTy -> 
  R.bind(inferTypeExpr ctx expr) (fun recTy ->
         match recTy with
         | RecordType (_, flds) ->
            R.bind (try R.ok (List.find (fun (_, i, _) -> i = idx) flds) with
            | Not_found -> typeError pos ("Cannot project field " ^ idx
                                          ^ " from given record type "
                                          ^ Lib.string_of_t LA.pp_print_lustre_type recTy)) (fun (_, _, fty) -> 
                R.bind (eqLustreType ctx fty expTy) (fun isEq ->
                    if isEq
                    then R.ok ()
                    else typeError pos ("Cannot match expected type "
                                        ^ Lib.string_of_t LA.pp_print_lustre_type expTy
                                        ^ " with infered type "
                                        ^ Lib.string_of_t LA.pp_print_lustre_type fty)))
         | _ -> typeError pos ("Cannot project field " ^ idx
                               ^ " from a non record type "
                               ^ Lib.string_of_t LA.pp_print_lustre_type recTy))       

and checkTypeConstDecl: tcContext -> LA.const_decl -> tcType -> unit tcResult =
  fun ctx constDecl expTy ->
  match constDecl with
  | FreeConst (pos, i, lusTy) ->
     let infTy = (lookupTy ctx i) in
     if infTy != expTy
     then typeError pos
            ("Variable "
             ^ i
             ^ " expected to have type " ^ string_of_tcType infTy
             ^ " but found type " ^ string_of_tcType expTy)
     else R.ok ()
  | UntypedConst (pos, i, exp) ->
     R.bind (inferTypeExpr ctx exp) (fun ty ->
         if expTy != ty
         then typeError pos
                ("Variable "
                 ^ i
                 ^ " expected to have type " ^ string_of_tcType expTy
                 ^ " but found type " ^ string_of_tcType ty)
         else R.ok ())     
  | TypedConst (pos, i, exp, lusTy) ->
     R.bind (inferTypeExpr ctx exp) (fun infTy ->
         if expTy != infTy
         then typeError pos
                ("Variable "
                 ^ i
                 ^ " expects type " ^ string_of_tcType expTy
                 ^ " but expression is of type " ^ string_of_tcType infTy)
         else R.ok ())  

and checkTypeNodeDecl: tcContext -> LA.node_decl -> tcType -> unit tcResult
  = fun ctx
        (nodeName, isExtern, params, cclktydecls, clktydecls, localdecls, items, contract)
        expTy ->
  let localVarBinding: tcContext -> LA.node_local_decl -> tcContext tcResult = fun ctx ->
    function
    | LA.NodeConstDecl (pos, constDecls) ->
       Log.log L_debug "Extracting typing context from const declaration: %a"
         LA.pp_print_const_decl constDecls
      ; tcCtxConstDecl ctx constDecls 
    | LA.NodeVarDecl (pos, (_, v, ty, _)) ->
       if isTypeWellFormed ctx ty then R.ok (addTy ctx v ty)
       else typeError pos ("Node's local variable "
                           ^ v
                           ^ " type should be well formed") in

  Log.log L_debug "TC declaration node: %a {" LA.pp_print_ident nodeName
  (* if the node is extern, we will not have any body to typecheck *)
  ; if isExtern
    then ( Log.log L_debug "External Node, no body to type check"
         ; R.ok ())
    else (
      Log.log L_debug "Params: %a (skipping)" LA.pp_print_node_param_list params
    ; (* store the input constants passed in the input *)
      let ipConstantsCtx = List.fold_left union ctx (List.map extractConsts cclktydecls) in
      (* These are inputs to the node *)
      let ctxPlusIps = List.fold_left union ipConstantsCtx (List.map extractArgCtx cclktydecls) in
      (* These are outputs of the node *)
      let ctxPlusOpsAndIps = List.fold_left union ctxPlusIps (List.map extractRetCtx clktydecls) in
        R.bind (R.seq (List.map (localVarBinding ctxPlusIps) localdecls)) (fun localVarCtxts ->
              (* Local TC context is input vars + output vars + local const and var decls *)
              let localCtx = List.fold_left union ctxPlusOpsAndIps localVarCtxts in
              Log.log L_debug "Local Typing Context {%a}" pp_print_tcContext localCtx
              (* Type check the node items now that we have all the local typing context *)
              ; R.bind (R.seq_ (List.map (doItem localCtx) items)) (fun _ -> 
                    R.ok (Log.log L_debug "TC declaration node %a done }" LA.pp_print_ident nodeName))))

and doNodeEqn: tcContext -> LA.node_equation -> unit tcResult = fun ctx ->
  function
  | LA.Assert (pos, e) ->
     Log.log L_debug "Checking assertion: %a" LA.pp_print_expr e
    ; R.bind (checkTypeExpr ctx e (Bool pos)) (fun _ -> R.ok ())
  | LA.Equation (_, lhs, expr)  as eqn ->
     Log.log L_debug "Checking equation: %a" LA.pp_print_node_body eqn
    (* This is a special case where we have undeclared identifiers 
       as short hands for assigning values to arrays *)
    ; let getArrayDefContext: LA.struct_item -> tcContext = 
        function
        | ArrayDef (pos, _, is) -> List.fold_left (fun c i -> addTy c i (LA.Int pos)) emptyContext is 
        | _ -> emptyContext
      in
      let ctxFromLHS: tcContext -> LA.eq_lhs -> tcContext = fun ctx (LA.StructDef (_, items))->
        List.fold_left union ctx (List.map getArrayDefContext items) 
      in
      R.bind (inferTypeExpr (ctxFromLHS ctx lhs) expr) (fun ty ->
          checkTypeStructDef (ctxFromLHS ctx lhs) lhs ty)
  | LA.Automaton (pos, _, _, _) ->
     Log.log L_debug "Skipping Automation"
       ; R.ok ()

and doItem: tcContext -> LA.node_item -> unit tcResult = fun ctx ->
  function
  | LA.Body eqn -> doNodeEqn ctx eqn
  | LA.AnnotMain _ as ann ->
     Log.log L_debug "Node Item Skipped (Main Annotation): %a" LA.pp_print_node_item ann
    ; R.ok ()
  | LA.AnnotProperty (_, _, e) as ann ->
     Log.log L_debug "Node Item (Annotation Property): %a" LA.pp_print_node_item ann
    ; checkTypeExpr ctx e (Bool (LH.pos_of_expr e))

  
and checkTypeStructItem: tcContext -> LA.struct_item -> tcType -> unit tcResult
  = fun ctx st expTy ->
  match st with
  | SingleIdent (pos, i) ->
     let infTy = lookupTy ctx i in
     R.bind (eqLustreType ctx expTy infTy) (fun isEq ->
         if isEq
         then R.ok ()
         (*This is an ugly fix to try and see if the RHS was instead a function return call *)
         else R.bind (eqLustreType ctx expTy (TupleType (pos, (infTy::[])))) (fun isEq' ->
                  if isEq'
                  then R.ok()
                  else typeError pos ("Expected type "
                                      ^ string_of_tcType expTy
                                      ^ " but found type "
                                      ^ string_of_tcType infTy)))
  | ArrayDef (pos, baseE, idxs) ->
     let arrayIdxExpr =
       List.fold_left (fun e i -> LA.ArrayIndex (pos, e, i))
         (LA.Ident (pos, baseE))
         (List.map (fun i -> LA.Ident (pos, i)) idxs) in
     checkTypeExpr ctx arrayIdxExpr expTy
  | TupleStructItem _ -> Lib.todo __LOC__
  | TupleSelection _ -> Lib.todo __LOC__
  | FieldSelection _ -> Lib.todo __LOC__
  | ArraySliceStructItem _ -> Lib.todo __LOC__

(** The structure of the left hand side of the equation 
 * should match the type of the right hand side expression *)
and checkTypeStructDef: tcContext -> LA.eq_lhs -> tcType -> unit tcResult
  = fun ctx (StructDef (pos, lhss)) expTy ->
  (* This is a structured type, and we would want the expected type expTy to be a tuple type *)
    (Log.log L_debug "TypeStructDef checking if %a has type %a"
       (Lib.pp_print_list LA.pp_print_struct_item ",")
       lhss LA.pp_print_lustre_type expTy
    ; match expTy with
      | TupleType (_, expTyLst) ->
         if List.length lhss = List.length expTyLst
         then R.seq_ (List.map2 (checkTypeStructItem ctx) lhss expTyLst)
         else typeError pos ("Term structure on left hand side of the equation"
                             ^ " does not match expected type "
                             ^ Lib.string_of_t LA.pp_print_lustre_type expTy 
                             ^ " on right hand side of the node equation")

      (* We are dealing with simple types, so lhs has to be a singleton list *)
      | _ -> if (List.length lhss != 1)
             then typeError pos ("Term structure on left hand side of the equation"
                                 ^ " does not match expected type structure "
                                 ^ Lib.string_of_t LA.pp_print_lustre_type expTy 
                                 ^ " on right hand side of the node equation")
             else let lhs = List.hd lhss in
                  checkTypeStructItem ctx lhs expTy)


and extractContractEqnContext: tcContext -> LA.contract_node_equation -> tcContext tcResult =
  fun ctx -> function
  | GhostConst c -> tcCtxConstDecl ctx c 
  | GhostVar c -> tcCtxConstDecl ctx c
  | Assume (pos, _, _, e) -> R.ok ctx
  | Guarantee _ -> R.ok ctx
  | Mode (pos, name, _, _) -> R.ok (addTy ctx name (Bool pos)) 
  | ContractCall _ -> R.ok ctx
  
and checkTypeContractDecl: tcContext -> LA.contract_node_decl -> unit tcResult =
  fun ctx (cname, params, args, rets, contract) ->
  Log.log L_debug "TC Contract decl: %a {" LA.pp_print_ident cname 
  (* build the appropriate local context *)
  ; let argCtx = List.fold_left union ctx (List.map extractArgCtx args) in
  let retCtx = List.fold_left union argCtx (List.map extractRetCtx rets) in
  let localCnstCtx = List.fold_left union retCtx (List.map extractConsts args) in
  (* get the local const var declarations into the context *)
  R.bind (R.seq (List.map (extractContractEqnContext localCnstCtx) contract)) (fun ctxs -> 
      let localCtx = List.fold_left union localCnstCtx ctxs in
      R.bind (checkTypeContract localCtx contract) (fun _ -> 
          R.ok (Log.log L_debug "TC Contract Decl %a done }" LA.pp_print_ident cname)))
  
and checkTypeContract: tcContext -> LA.contract -> unit tcResult =
  fun ctx eqns ->
  R.seq_ (List.map (checkContractNodeEqn ctx) eqns)

and checkContractNodeEqn: tcContext -> LA.contract_node_equation -> unit tcResult =
  fun ctx eqn ->
  Log.log L_debug "checking contract with equation %a" LA.pp_print_contract_item eqn 
  ; match eqn with
  | GhostConst _
  | GhostVar _ ->  R.ok () (* This is already checked while extracting ctx *)
  | Assume (pos, _, _, e) -> checkTypeExpr ctx e (Bool pos)
  | Guarantee (pos, _, _, e) -> checkTypeExpr ctx e (Bool pos)
  | Mode (pos, _, reqs, ensures) ->
     R.seq_ (Lib.list_apply (List.map (checkTypeExpr ctx)
                               (List.map (fun (_,_, e) -> e) (reqs @ ensures)))
               (Bool pos)) 
  | ContractCall (pos, cname, e1s, e2s) -> (* of contract_call *) Lib.todo __LOC__

                                           
and tcCtxOfTyDecl: tcContext -> LA.type_decl -> tcContext tcResult = fun ctx ->
  function
  | LA.AliasType (_, i, ty) ->
     (match ty with
      | LA.EnumType (pos, n, econsts) ->
         (match n with
          | None -> typeError pos "Necessary Enum name not found"
          | Some ename -> R.ok (List.fold_left union ctx
                                  (List.map ((Lib.flip singletonTy) (LA.UserType (pos, ename))) econsts)))
      | _ -> R.ok (addTySyn ctx i ty))
  | LA.FreeType _ -> R.ok ctx

(** get the type signature of node or a function *)
and tcCtxOfNodeDecl: Lib.position -> tcContext -> LA.node_decl -> tcContext tcResult
  = fun pos ctx (nname, _, _ , ip, op,_ ,_ ,_) ->
  Log.log L_debug
    "Extracting typing context from node declaration: %a"
    LA.pp_print_ident nname
  ; if (memberTy ctx nname)
    then typeError pos ("Duplicate node detected with name: " ^ nname)
    else R.bind (buildNodeFunTy pos ctx ip op) (fun funTy ->
             R.ok (addTy ctx nname funTy))

and tcCtxOfContractNodeDecl: Lib.position -> tcContext
                             -> LA.contract_node_decl -> tcContext tcResult =
  fun pos ctx (cname, params, inputs, outputs, contrat) ->
  Log.log L_debug
    "Extracting typing context from contract declaration: %a"
    LA.pp_print_ident cname
  ; if (memberTy ctx cname)
    then typeError pos ("Duplicate node detected with name: " ^ cname)
    else R.bind (buildNodeFunTy pos ctx inputs outputs) (fun funTy ->
             R.ok (addTy ctx cname funTy))

(** Obtain a global typing context, get constants and function decls*)
and tcContextOf: tcContext -> LA.t -> tcContext tcResult = fun ctx ->
  let rec tcContextOf': tcContext -> LA.declaration -> tcContext tcResult = fun ctx ->
    function
    | LA.TypeDecl (_, tyDecl)     -> tcCtxOfTyDecl ctx tyDecl 
    | LA.ConstDecl (_, constDecl) -> tcCtxConstDecl ctx constDecl
    | LA.NodeDecl (pos, nodeDecl) -> tcCtxOfNodeDecl pos ctx nodeDecl
    | LA.FuncDecl (pos, nodeDecl) -> tcCtxOfNodeDecl pos ctx nodeDecl
    | LA.ContractNodeDecl (pos, contractDecl) -> tcCtxOfContractNodeDecl pos ctx contractDecl
    | _ -> R.ok ctx
    in function
    | [] -> R.ok ctx
    | d :: tl ->
       R.bind (tcContextOf' ctx d) (fun ctx' ->
           R.bind (tcContextOf (union ctx' ctx) tl) (fun c -> 
               R.ok c))

(** Does it make sense to have this type i.e. is it inhabited? 
 * We do not want types such as int^true *)
and isTypeWellFormed: tcContext -> tcType -> bool = fun ctx ty ->
  match ty with
  | LA.TArr (_, argTy, resTy) -> isTypeWellFormed ctx argTy
                                 && isTypeWellFormed ctx resTy
  | LA.RecordType (_, idTys) -> List.fold_left (&&) true
                                  (List.map (fun (_, _, ty)
                                             -> isTypeWellFormed ctx ty) idTys)
  | LA.ArrayType (_, (_, s)) -> isExprIntType ctx s
                                && isExprOfConts ctx s
  | LA.TupleType (_, tys) -> List.fold_left (&&) true
                               (List.map (isTypeWellFormed ctx) tys)
  | _ -> true
                   
(** Shadow the old binding with the new const decl *)
and tcCtxConstDecl: tcContext -> LA.const_decl -> tcContext tcResult = fun ctx ->
  function
  | LA.FreeConst (pos, i, ty) ->
     if (isTypeWellFormed ctx ty)
     then R.ok (addTy ctx i ty)
     else typeError pos "Constant should be of a well formed type"
  | LA.UntypedConst (_, i, e) ->
     R.bind (inferTypeExpr ctx e) (fun ty -> 
         R.ok (addTy (addConst ctx i e ty) i ty))
  | LA.TypedConst (pos, i, expr, expTy) ->
     R.bind (checkTypeExpr (addTy ctx i expTy) expr expTy) (fun _ ->
         R.ok (addTy (addConst ctx i expr expTy) i expTy))
  
(** Function type for nodes will be (TupleTy ips) -> (TupleTy outputs)  *)
and buildNodeFunTy: Lib.position -> tcContext -> LA.const_clocked_typed_decl list
                    -> LA.clocked_typed_decl list -> tcType tcResult
  = fun pos ctx args rets ->
  let funConstCtx = List.fold_left (fun ctx (i,ty) -> addConst ctx i (LA.Ident (pos,i)) ty)
                      ctx (List.filter isConstArg args |> List.map extractIpTy) in
  let funCtx = List.fold_left (fun ctx (i, ty)-> addTy ctx i ty) funConstCtx (List.map extractIpTy args) in   
  let ops = List.map snd (List.map extractOpTy rets) in
  let ips = List.map snd (List.map extractIpTy args) in
  let retTy = LA.TupleType (pos, ops) in
  let argTy = LA.TupleType (pos, ips) in
  if isTypeWellFormed funCtx retTy && isTypeWellFormed funCtx argTy
  then R.ok (LA.TArr (pos, argTy, retTy))
  else typeError pos "Return type and argument type of node should be well formed."

(** Compute Equality for lustre types  *)
and eqLustreType : tcContext -> LA.lustre_type -> LA.lustre_type -> bool tcResult = fun ctx t1 t2 ->
  match (t1, t2) with

  (* Type Variable *)
  | TVar (_, i1), TVar (_, i2) -> R.ok (i1 = i2)

  (* Simple types *)
  | Bool _, Bool _ -> R.ok true
  | Int _, Int _ -> R.ok true
  | UInt8 _, UInt8 _ -> R.ok true
  | UInt16 _, UInt16 _ -> R.ok true              
  | UInt32 _, UInt32 _ -> R.ok true
  | UInt64 _,UInt64 _ -> R.ok true 
  | Int8 _, Int8 _ -> R.ok true 
  | Int16 _, Int16 _ -> R.ok true
  | Int32 _, Int32 _ -> R.ok true
  | Int64 _, Int64 _ -> R.ok true
  | Real _, Real _ -> R.ok true

  (* Integer Range *)
  | IntRange _, IntRange _ -> R.ok true

  (* Lustre V6 features *)
  | UserType (_, i1), UserType (_, i2) -> R.ok (i1 = i2)
  | AbstractType (_, i1), AbstractType (_, i2) -> R.ok (i1 = i2)
  | TupleType (_, tys1), TupleType (_, tys2) ->
     if List.length tys1 = List.length tys2
     then R.bind (R.seq (List.map2 (eqLustreType ctx) tys1 tys2)) (fun isEqs ->
              R.ok (List.fold_left (&&) true isEqs))
     else R.ok false
  | RecordType (_, tys1), RecordType (_, tys2) ->
     R.bind (R.seq (List.map2 (eqTypedIdent ctx)
                      (sortTypedIdent tys1)
                      (sortTypedIdent tys2))) (fun isEqs ->
        R.ok (List.fold_left (&&) true isEqs))
  | ArrayType (pos1, arr1), ArrayType (pos2, arr2) -> eqTypeArray ctx arr1 arr2 
  | EnumType (_, n1, is1), EnumType (_, n2, is2) ->
     R.ok (n1 = n2 && (List.fold_left (&&) true (List.map2 (=) (sortIdents is1) (sortIdents is2))))
  (* node/function type *)
  | TArr (_, argTy1, retTy1), TArr (_, argTy2, retTy2) ->
     R.bind (eqLustreType ctx argTy1 argTy2) (fun isEqArgTy ->
         if isEqArgTy
         then eqLustreType ctx retTy1 retTy2
         else R.ok false )
  (* special case for type synonyms *)
  | UserType (_, u), ty
    | ty, UserType (_, u) ->
     if memberTySyn ctx u
     then let tyAlias  = lookupTySyn ctx u in 
          eqLustreType ctx ty tyAlias
     else R.ok false
  (* Another special case for tuple equality type *)
  | TupleType (_, tys), t
    | t, TupleType (_, tys) -> if List.length tys = 1 then (eqLustreType ctx (List.hd tys) t) else R.ok false  
  | _, _ -> R.ok false

(** Checks if the constant is of type Int. This will be useful 
 * in evaluating array sizes that we need to have as constant integers
 * while declaring the array type *)
and isExprIntType: tcContext -> LA.expr -> bool  = fun ctx e ->
  R.safe_unwrap false (
      R.bind (inferTypeExpr ctx e) (fun ty -> 
          eqLustreType ctx ty (LA.Int (LH.pos_of_expr e))))

and isExprBoolType:  tcContext -> LA.expr -> bool = fun ctx e ->
  R.safe_unwrap false (
      R.bind (inferTypeExpr ctx e) (fun ty -> 
          eqLustreType ctx ty (LA.Bool (LH.pos_of_expr e))))

and isExprArrayType: tcContext -> LA.expr -> bool  = fun ctx e ->
  R.safe_unwrap false
    (R.bind (inferTypeExpr ctx e) (fun ty ->
         match ty with
         | ArrayType _ -> R.ok true
         | _ -> R.ok false))

(** checks if all the variables in the expression are constants *)
and isExprOfConts: tcContext -> LA.expr -> bool = fun ctx e ->
  List.fold_left (&&) true (List.map (memberVal ctx) (LA.SI.elements (LH.vars e)))
  
(** Compute type equality for [LA.typed_ident] *)
and eqTypedIdent: tcContext -> LA.typed_ident -> LA.typed_ident -> bool tcResult =
  fun ctx (_, i1, ty1) (_, i2, ty2) -> eqLustreType ctx ty1 ty2

(** Compute equality for [LA.ArrayType] *)
and eqTypeArray: tcContext -> (LA.lustre_type * LA.expr) -> (LA.lustre_type * LA.expr) -> bool tcResult
  = fun ctx (ty1, e1) (ty2, e2) -> eqLustreType ctx ty1 ty2
(** For now, we do not check the bounds for arrays. This introduces bugs similar to Issue #566. 
    https://github.com/kind2-mc/kind2/issues/566. 
    Backend should handle such cases as it can talk to a powerful solver. *)
                                 
(** Compute the strongly connected components for type checking *)
let scc: LA.t -> LA.t list
  = fun decls -> [decls]
               
(** By this point, all the circularity should be resolved,
 * the top most declaration should be able to access 
 * the types of all the forward referenced indentifiers from the context*)
let rec typeCheckGroup: tcContext -> LA.t ->  unit tcResult list
  = fun globalCtx
  -> function
  | [] -> [R.ok ()]
  (* skip over type declarations and constDecls*)
  | (LA.TypeDecl _ :: rest) 
    | LA.ConstDecl _ :: rest -> typeCheckGroup globalCtx rest  
  | LA.NodeDecl (_, ((i, _,_, _, _, _, _, _) as nodeDecl)) :: rest ->
     (checkTypeNodeDecl globalCtx nodeDecl (lookupTy globalCtx i))
     :: typeCheckGroup globalCtx rest
  | LA.FuncDecl (_, ((i, _,_, _, _, _, _, _) as nodeDecl)):: rest ->
     (checkTypeNodeDecl globalCtx nodeDecl (lookupTy globalCtx i))
     :: typeCheckGroup globalCtx rest
  | LA.ContractNodeDecl (_, ((i, _, _, _, _) as contractDecl)) :: rest ->
     (checkTypeContractDecl globalCtx contractDecl)
                                     :: typeCheckGroup globalCtx rest
  | LA.NodeParamInst  _ :: rest -> Lib.todo __LOC__
       
       
(** Typecheck a list of independent groups using a global context*)
let typeCheckDeclGrps: tcContext -> LA.t list -> unit tcResult list = fun ctx decls -> 
  List.concat (List.map (typeCheckGroup ctx) decls)               

(** Get the first error *)
let rec reportTcResult: unit tcResult list -> unit tcResult = function
  | [] -> R.ok () 
  | Error (pos,err) :: _ -> LC.fail_at_position pos err
  | Ok () :: tl -> reportTcResult tl

(****************************************************************
 * The main function of the file that kicks type checking flow  *
 ****************************************************************)  
let typeCheckProgram: LA.t -> unit tcResult = fun prg ->
  R.bind (Log.log L_debug ("===============================================\n"
                           ^^ "Phase 1: Building TC Global Context\n"
                           ^^"===============================================\n")
        ; tcContextOf emptyContext prg) (fun tcCtx ->
      Log.log L_debug ("===============================================\n"
                       ^^ "Phase 1: Completed Building TC Global Context\n"
                       ^^ "TC Context\n%a\n"
                       ^^"===============================================\n")
        pp_print_tcContext  tcCtx
      ; prg |> scc |> typeCheckDeclGrps tcCtx |> reportTcResult)
(** Typechecks the [LA.declaration list] or the lustre program Ast and returns 
 *  a [Ok ()] if it succeeds or and [Error of String] if the typechecker fails*)
    
    
(* 
   Local Variables:
   compile-command: "make -k -C .."
   indent-tabs-mode: nil
   End: 
*)

