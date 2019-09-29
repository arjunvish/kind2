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

(** Convert a NuXmv Modules to a transition system

    {1 Translation}

    Each Vmt expression list is translated by first defining
    all the possible sorts of the file specified by the 
    "DefineSort" function.

    Then the state variables are determined by looking through
    the expressions and locating all the DefineFun expressions
    that contained an attribute term in them with the attribute 
    next. After figuring out which varaibles in the file were 
    next vars, the state var isntance map was created to replace
    all instances of these state vars with the corresponding
    instance value. This also generated the list of state vars
    for the translation.

    Following this, the init and trans term were generated.
    This was done by locating the DefineFun which contained
    the attribute init true or trans true respectively. After 
    this expression was found, each variable instance was 
    replaced by the correct term to generate one long expression
    for both the init and trans term. The same was done for the
    properties, but trying to locate the attribute terms 
    for the properties.

    At this point, all the needed information for the 
    transition system has been gathered, and we require
    the standard process of creating all the inputs for
    the {!TransSys.mk_trans_sys} method to be called to
    generate the transisiton system we return.
    
    @author Andrew West *)

val trans_sys_of_vmt : 
    ?preserve_sig:bool ->
    ?slice_nodes:bool -> 
    VmtAst.t SubSystem.t-> Analysis.param ->
    TransSys.t * VmtAst.t SubSystem.t