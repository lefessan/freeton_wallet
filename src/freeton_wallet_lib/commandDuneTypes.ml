(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

type swap = {
  swap_id : int ;
  swap_dun_addr : string ;
  swap_dun_amount : int64 ;
  swap_ton_amount : int64 ;
  swap_ton_addr : string ;
  swap_ton_pubkey : string ;
} [@@deriving json_encoding]

type transfer = {
  tr_name : string option ;
  tr_id : int ;
  mutable tr_swaps : swap list ;
  mutable tr_ton_amount : int64 ;
  mutable tr_ton_addr : string ;
  mutable tr_ton_pubkey : string ;
  mutable tr_ready : bool ; [@dft false]
} [@@deriving json_encoding]

type batch = {
  batch_id : int ;
  batch_transfers : transfer list;
  batch_amount : int64 ;
  batch_frozen : bool ;
  batch_ready : bool ;
} [@@deriving json_encoding]

module Int64 = struct
  include Int64
  let of_string s = try of_string s with
    | exn ->
        raise (Failure (Printf.sprintf "Int64.of_string(%s):%s" s
                          ( Printexc.to_string exn)))
  module OP = struct
    let (++) = Int64.add
    let (--) = Int64.sub
    let (//) = Int64.div
    let ( ** ) = Int64.mul
  end
end
