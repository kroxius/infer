(* Author: Lukáš Trecha <xtrechl00@stud.fit.vutbr.cz> *)
(* TEST FILE *)

(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

include AbstractDomain.S

val initial : t

val mem_malloc : t -> t

val mem_free : t -> t

val has_leak : t -> bool

val apply_summary : summary:t -> t -> t

val record_allocates : t -> t

type summary = t
