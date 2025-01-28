(* Author: Lukáš Trecha <xtrechl00@stud.fit.vutbr.cz> *)
(* TEST FILE *)

(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val checker :
  BugFinderDomain.summary InterproceduralAnalysis.t -> BugFinderDomain.summary option
