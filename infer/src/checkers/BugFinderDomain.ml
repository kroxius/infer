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

module FiniteBounds = struct
  type t = int

  let leq ~lhs ~rhs = lhs <= rhs

  let join a b = max a b

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt astate = F.fprintf fmt "%d" astate
end

module BoundsWithTop = struct
  open AbstractDomain.Types
  include AbstractDomain.TopLifted (FiniteBounds)

  let widening_threshold = 5

  let widen ~prev ~next ~num_iters =
    match (prev, next) with
    | Top, _ | _, Top -> Top
    | NonTop prev, NonTop next ->
      if num_iters < widening_threshold
        then NonTop (FiniteBounds.join prev next)
        else Top
end

module AllocatesMemory = AbstractDomain.BooleanOr
include AbstractDomain.Pair (BoundsWithTop) (AllocatesMemory)
open AbstractDomain.Types

let initial = (NonTop 0, false)

let mem_malloc astate =
  match astate with
  | (Top, bor) -> (Top, bor)
  | (NonTop cnt, bor) -> (NonTop (cnt+1), bor)

let mem_free astate =
  match astate with
  | (Top, bor) -> (Top, bor)
  | (NonTop cnt, bor) -> (NonTop (cnt-1), bor)

let has_leak astate =
  match astate with
  | (Top, _) -> true
  | (NonTop cnt, _) -> if cnt > 0 then true else false

let apply_summary ~summary:(summary_count, summary_allocates) (current_count, current_allocates) =
  let new_count =
    match current_count with
    | Top -> Top
    | NonTop current_count ->
      let return_count = if summary_allocates then 1 else 0 in
      let summary_count =
        match summary_count with
        | Top -> 0
        | NonTop x -> x
      in NonTop (current_count + return_count + summary_count)
  in (new_count, current_allocates)

let record_allocates (cnt, _) = (cnt, true)   

type summary = t
 