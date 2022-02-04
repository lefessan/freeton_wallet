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

open EzCompat
(* open EzFile.OP *)
open Ezcmd.V2
open EZCMD.TYPES

let destruct name enc s =
  match EzEncoding.destruct enc s with
  | exception exn ->
      Printf.eprintf "Cannot parse JSON:\n%s\n%!" s;
      Printf.eprintf "Error: %s\n%!" (Printexc.to_string exn);
      Error.raise "Cannot parse %s" name
    | t -> t

open CommandDuneTypes

include Int64.OP

module TYPES = struct
  type ctxt = {
    config : Types.config;
    net : Types.network;
    client : Sdk_types.client ;
    server_url : string ;
    airdrop_contract_abi : string ;
    batch_address : string ;
    keypair : Sdk_types.keypair option ;
  }
end
open TYPES

module GET = struct
  type transferInfo = {
    ton_amount : string ;
    dest_addr : string ;
    dest_pubkey : string ;
    vesting_addr : string ;
  } [@@deriving json_encoding]

  type batch = {
    current_batch : transferInfo array ;
    giver_addr : string ;
    batch_size : string ;
    ntransfers : string ;
    nprocessed : string ;
    ton_amount : string ;
    ton_required : string ;
  } [@@deriving json_encoding]

end

let get_context ?src account =
  let config = Config.config () in
  let net = Config.current_network config in
  let client, server_url =
    let node = Config.current_node config in
    let client = Ton_sdk.CLIENT.create node.node_url in
    let server_url = node.node_url in
    client, server_url
  in
  let key = Misc.find_key_exn net account in
  let batch_address = Misc.get_key_address_exn key in
  let airdrop_contract_abifile =
    Misc.get_contract_abifile "DuneAirdrop" in
  let airdrop_contract_abi = EzFile.read_file airdrop_contract_abifile in
  let keypair = match src with
    | None -> None
    | Some src ->
        let key = Misc.find_key_exn net src in
        Some (Misc.get_key_pair_exn key)
  in
  {
    config;
    net;
    client;
    server_url;
    airdrop_contract_abi;
    batch_address ;
    keypair ;
  }


let batch_of_reply ~reply =
  let batch = destruct "batch_of_reply" GET.batch_enc reply in
  (*
  let open GET in
  let transfers = List.map (fun t ->
      { t with
        ton_amount = int_of_string t.ton_amount ;
      }
    ) transfers in
  { transfers }
*)
  batch

let call_addTransfer ctxt tr =
  let params = Printf.sprintf {|
      {
      "ton_amount" : "%Ld",
      "dest_addr": "%s",
      "dest_pubkey": "0x%s"
      }
|}
      tr.tr_ton_amount
      tr.tr_ton_addr
      tr.tr_ton_pubkey
  in
  let reply =
    Utils.call_run
      ctxt.config
      ~client:ctxt.client
      ~wait:false
      ~server_url:ctxt.server_url
      ~address: ctxt.batch_address
      ~abi:ctxt.airdrop_contract_abi
      ~meth:"addTransfer"
      ~params
      ~local:false
      ?keypair:ctxt.keypair
      ()
  in
  Printf.eprintf "reply: %s\n%!" reply;
  ()

let batch_get ctxt =
  let reply =
    Utils.call_run
      ctxt.config
      ~client:ctxt.client
      ~wait:false
      ~server_url:ctxt.server_url
      ~address: ctxt.batch_address
      ~abi:ctxt.airdrop_contract_abi
      ~meth:"get"
      ~params:"{}"
      ~local:true
      ()
  in
  Printf.eprintf "reply: %s\n%!" reply;
  batch_of_reply ~reply


let action ?src ~batch_addr batch_id =

  let batch_file = Printf.sprintf "batch-%d.json" batch_id in
  let batch = Misc.read_json_file batch_enc batch_file in

  if batch.batch_ready then
    Error.raise "Batch already done";

  let transfers =
    let map = ref StringMap.empty in
    List.iter (fun tr ->
        let key = String.lowercase_ascii (
            tr.tr_ton_addr ^ ":0x" ^ tr.tr_ton_pubkey ) in
        if StringMap.mem key !map then
          Error.raise "duplicate transfer target";
        map := StringMap.add key tr !map
      ) batch.batch_transfers ;
    !map
  in

  let ctxt = get_context ?src batch_addr in

  let transfers = ref transfers in
  let transfers_ready = ref StringMap.empty in
  let rec iter () =
    let info = batch_get ctxt in

    for i = 0 to int_of_string info.ntransfers-1 do
      let ti = info.current_batch.(i) in
      let key = ti.dest_addr ^ ":" ^  ti.dest_pubkey in
      match StringMap.find key !transfers_ready with
      | _ -> ()
      | exception Not_found ->
          match StringMap.find key !transfers with
          | exception Not_found -> assert false
          | tr ->
              assert ( Int64.to_string tr.tr_ton_amount = ti.ton_amount );
              transfers_ready := StringMap.add key tr !transfers_ready;
              transfers := StringMap.remove key !transfers
    done;
    if not ( StringMap.is_empty !transfers ) then
      let () = () in
      StringMap.iter (fun _ tr ->
          call_addTransfer ctxt tr
        ) !transfers;
      iter ()
  in
  iter ();
  let batch = { batch with batch_ready = true } in
  Misc.write_json_file batch_enc batch_file batch;

  ()

let cmd =
  let batch_id = ref None in
  let batch_addr = ref None in
  let src = ref None in
  Misc.cmd
    ["dune"; "batch"]
    (fun () ->
       match !batch_id with
       | None -> Error.raise "You must specify the batch number"
       | Some batch_id ->
           match !batch_addr with
           | None -> Error.raise "You must specify the batch address"
           | Some batch_addr ->
               action batch_id ~batch_addr ?src:!src
    )
    ~args:
      [
        [ "id" ], Arg.Int (fun s -> batch_id := Some s),
        EZCMD.info ~docv:"NUM" "Batch number";

        [ "addr" ], Arg.String (fun s -> batch_addr := Some s),
        EZCMD.info ~docv:"ADDRESS" "Batch address";

        [ "src" ], Arg.String (fun s -> src := Some s),
        EZCMD.info ~docv:"ACCOUNT" "Signer";
      ]
    ~doc: "Prepare a dune batch"
    ~man:[
    ]
