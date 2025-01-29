(* Author: Lukáš Trecha <xtrechl00@stud.fit.vutbr.cz> *)
(* TEST FILE *)

(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Domain = BugFinderDomain

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = Domain.t InterproceduralAnalysis.t

  let exec_instr (astate : BugFinderDomain.t)
      {InterproceduralAnalysis.proc_desc= _; tenv=_; analyze_dependency; _} _ _ (instr : Sil.instr)
      =
    match instr with
    | Call (_return, Const (Cfun callee_proc_name), _actuals, _loc, _) -> (
        if (phys_equal (String.compare (Procname.to_string callee_proc_name) "malloc") 0) then BugFinderDomain.mem_malloc astate
        else if (phys_equal (String.compare (Procname.to_string callee_proc_name) "free") 0) then BugFinderDomain.mem_free astate
        else match analyze_dependency callee_proc_name with
        | Ok callee_summary -> BugFinderDomain.apply_summary ~summary:callee_summary astate
        | Error _ -> astate
    )
    | _ -> astate

  let pp_session_name _node fmt = F.pp_print_string fmt "bug finder test"
end

module CFG = ProcCfg.Normal
module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (CFG))

let report_if_leak {InterproceduralAnalysis.proc_desc; err_log; _} post =
  if BugFinderDomain.has_leak post then
    let last_loc = Procdesc.Node.get_loc (Procdesc.get_exit_node proc_desc) in
    let message = F.asprintf "%a memory leaks" BugFinderDomain.pp post in
    Reporting.log_issue proc_desc err_log ~loc:last_loc BugFinder
      IssueType.bugfinder_error message

(* TODO - FormalMap ? *)
let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  let result = Analyzer.compute_post analysis_data ~initial:BugFinderDomain.initial proc_desc in
  Option.iter result ~f:(fun post -> report_if_leak analysis_data post) ;
  result
