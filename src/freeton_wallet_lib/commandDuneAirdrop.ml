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

(* open EzFile.OP *)
open Ezcmd.V2
open EZCMD.TYPES

(*
module Int64 = struct
  include Int64
  let of_string s = try
      of_string s
    with exn ->
      Printf.eprintf "Failure on %S\n%!" s ;
      raise exn
end

module TYPES = struct
  type ctxt = {
    config : Types.config;
    net : Types.network;
    manifest : Types.MANIFEST.t;
    client : Sdk_types.client ;
    server_url : string ;
    root_contract_abi : string ;
    wallet_contract_abi : string ;
    vault_address : string ;
    dexroot_address : string ;
    dexroot_contract_abi : string ;
    dexpair_contract_abi : string ;
  }
end
open TYPES

module GETWALLETADDRESS = struct
  type reply = {
    value0 : string
  } [@@deriving json_encoding]
end

module EXPECTEDEXCHANGE = struct
  type reply = {
    expected_amount : string; [@key "expected_amount" ]
    expected_fee : string; [@key "expected_fee" ]
  } [@@deriving json_encoding]
end

module GETPAIRTWALLETS = struct
  type reply = {
    left : string ;
    right : string ;
    lp : string option ;
  } [@@deriving json_encoding]
end

let destruct name enc s =
  match EzEncoding.destruct enc s with
  | exception exn ->
      Printf.eprintf "Cannot parse JSON:\n%s\n%!" s;
      Printf.eprintf "Error: %s\n%!" (Printexc.to_string exn);
      Error.raise "Cannot parse %s" name
    | t -> t

let manifest =
  lazy (
    let manifest_file = Globals.ft_dir // "tokens.json" in
    let manifest =
      if Sys.file_exists manifest_file then
        EzFile.read_file manifest_file
      else
        match Files.read "manifest.json" with
        | None -> assert false
        | Some manifest -> manifest
    in
    destruct "manifest file" Types.MANIFEST.enc manifest
  )

let get_context config =
  let net = Config.current_network config in
  let client, server_url =
    let node = Config.current_node config in
    let client = Ton_sdk.CLIENT.create node.node_url in
    let server_url = node.node_url in
    client, server_url
  in
  let root_contract_abi =
    Misc.get_contract_abifile "Broxus_RootTokenContract" in
  let root_contract_abi = EzFile.read_file root_contract_abi in
  let wallet_contract_abi =
    Misc.get_contract_abifile "Broxus_TONTokenWallet" in
  let wallet_contract_abi = EzFile.read_file wallet_contract_abi in

  let dexroot_address = Utils.address_of_account net "broxus-dex-root"
                        |> Misc.raw_address in
  let vault_address = Utils.address_of_account net "broxus-wton-vault"
                      |> Misc.raw_address in

  let dexroot_contract_abi =
    Misc.get_contract_abifile "Broxus_DexRoot" in
  let dexroot_contract_abi = EzFile.read_file dexroot_contract_abi in
  let dexpair_contract_abi =
    Misc.get_contract_abifile "Broxus_DexPairV4" in
  let dexpair_contract_abi = EzFile.read_file dexpair_contract_abi in
  let manifest = Lazy.force manifest in
  {
    config;
    net;
    manifest;
    client;
    server_url;
    root_contract_abi;
    vault_address ;
    wallet_contract_abi;
    dexroot_address ;
    dexroot_contract_abi ;
    dexpair_contract_abi ;
  }

let address_of_reply ~query ~reply =
  let address = (
    destruct (Printf.sprintf "%s reply" query) GETWALLETADDRESS.reply_enc reply
  ).value0
  in
  address

let string_of_amount_token amount token =
  if amount = "0" then
    Printf.sprintf "0 %s" token.Types.MANIFEST.token_symbol
  else
    let len = String.length amount in
    let ndecimals = token.Types.MANIFEST.token_decimals in
    let units, decimals =
      if len > ndecimals then
        String.sub amount 0 ( len - ndecimals ),
        String.sub amount ( len - ndecimals ) ndecimals
      else
        "0",
        String.make ( ndecimals - len ) '0' ^
        amount
    in
    Printf.sprintf "%s.%s %s"
      units decimals
      token.Types.MANIFEST.token_symbol


let get_token_wallet_address ctxt token address =
  let root_address = token.Types.MANIFEST.token_address in
  let params = Printf.sprintf {|{ "_answer_id": 1,
                                      "wallet_public_key_": 0,
                                      "owner_address_": "%s"}|}
      address
  in
  let reply =
    Utils.call_run
      ctxt.config
      ~client:ctxt.client
      ~wait:false
      ~server_url:ctxt.server_url
      ~address:root_address
      ~abi:ctxt.root_contract_abi
      ~meth:"getWalletAddress"
      ~params
      ~local:true
      ()
  in
  address_of_reply ~query:"getWalletAddress" ~reply

let get_token_by_symbol ctxt symbol =
  let rec iter = function
      [] ->
        Error.raise "token with symbol %S not found\n%!" symbol
    | token :: tokens ->

        if token.Types.MANIFEST.token_symbol = symbol then
          token
        else iter tokens
  in
  iter ctxt.manifest.Types.MANIFEST.tokens

let contract_exists ctxt address =
  match CommandAccountState.get_address_info ctxt.config (RawAddress address)
  with
  | None -> false
  | Some _ -> true

let get_token_balance_gas ctxt wallet_address =
  let info = CommandAccountState.get_address_info ctxt.config
      (RawAddress wallet_address) in
  match info with
  | None -> None
  | Some acc ->
      let gas =
        match acc.acc_balance with
        | None -> 0L
        | Some n -> Z.to_int64 n
      in
      let params = Printf.sprintf {|{ "_answer_id": 1 }|} in
      let reply =
        Utils.call_run ctxt.config
          ~client:ctxt.client
          ~wait:false
          ~server_url:ctxt.server_url
          ~address:wallet_address
          ~abi:ctxt.wallet_contract_abi
          ~meth:"balance"
          ~params
          ~local:true
          ()
      in
      let balance = (
        destruct "balance reply" GETWALLETADDRESS.reply_enc reply
      ).value0
      in
      Some ( balance, gas )

let print_wallet ctxt ~address ~wallet_address ~token =
  Printf.printf "wallet address: %s (for contract %s)\n%!"
    wallet_address address;
  match get_token_balance_gas ctxt wallet_address with
  | None ->
      Printf.printf "  Broxus_TONTokenWallet contract not yet deployed\n%!";
  | Some ( balance, gas ) ->
      Printf.printf "  balance %s (gas %s TON)\n%!"
        ( string_of_amount_token balance token )
        ( Misc.string_of_nanoton gas )

let print_wallets config account =
  let ctxt = get_context config in
  let key = Misc.find_key_exn ctxt.net account in
  let contract = CommandMultisigCreate.check_key_contract key in
  let address = Misc.get_key_address_exn key in
  Printf.printf "%s (at %s, %s):\n%!" account address contract;
  let manifest = Lazy.force manifest in

  List.iter (fun token ->

      let wallet_address = get_token_wallet_address ctxt token address in

      print_wallet ctxt ~address ~wallet_address ~token
    ) manifest.Types.MANIFEST.tokens


let get_dexpair_address ctxt token1 token2 =
  let from_token_root_address = token1.Types.MANIFEST.token_address in
  let to_token_root_address = token2.Types.MANIFEST.token_address in
  let params =
    Printf.sprintf {|{ "answerId": 1,
                       "left_root": "%s",
                       "right_root": "%s"}|}
      from_token_root_address
      to_token_root_address
  in
  let reply =
    Utils.call_run
      ctxt.config
      ~client:ctxt.client
      ~wait:false
      ~server_url:ctxt.server_url
      ~address:ctxt.dexroot_address
      ~abi:ctxt.dexroot_contract_abi
      ~meth:"getExpectedPairAddress"
      ~params
      ~local:true
      ()
  in
  address_of_reply ~query:"getExpectedPairAddress" ~reply

let get_dexpair_exchange_rate ctxt dexpair_address from_token =
  let from_token_root_address = from_token.Types.MANIFEST.token_address in
  let params =
    Printf.sprintf {|{ "answerId": 1,
                       "amount": "1000000000",
                       "spent_token_root": "%s"}|}
      from_token_root_address
  in
  let reply =
    Utils.call_run
      ctxt.config
      ~client:ctxt.client
      ~wait:false
      ~server_url:ctxt.server_url
      ~address:dexpair_address
      ~abi:ctxt.dexpair_contract_abi
      ~meth:"expectedExchange"
      ~params
      ~local:true
      ()
  in
  (* Printf.eprintf "reply: %s\n%!" reply; *)
  let reply = (
    destruct "expectedExchange reply"
      EXPECTEDEXCHANGE.reply_enc reply
  )
  in
  reply.EXPECTEDEXCHANGE.expected_amount,
  reply.expected_fee


let get_dexpair_vaults ctxt dexpair_address =
  let params =
    Printf.sprintf {|{ "answerId": 1 }|}
  in
  let reply =
    Utils.call_run
      ctxt.config
      ~client:ctxt.client
      ~wait:false
      ~server_url:ctxt.server_url
      ~address:dexpair_address
      ~abi:ctxt.dexpair_contract_abi
      ~meth:"getVaultWallets"
      ~params
      ~local:true
      ()
  in
  (* Printf.eprintf "reply: %s\n%!" reply; *)
  let reply = (
    destruct "getVaultWallets reply"
      GETPAIRTWALLETS.reply_enc reply
  )
  in
  reply.left, reply.right

let get_dexpair_wallets ctxt dexpair_address =
  let params =
    Printf.sprintf {|{ "answerId": 1 }|}
  in
  let reply =
    Utils.call_run
      ctxt.config
      ~client:ctxt.client
      ~wait:false
      ~server_url:ctxt.server_url
      ~address:dexpair_address
      ~abi:ctxt.dexpair_contract_abi
      ~meth:"getTokenWallets"
      ~params
      ~local:true
      ()
  in
  (* Printf.eprintf "reply: %s\n%!" reply; *)
  let reply = (
    destruct "getTokenWallets reply"
      GETPAIRTWALLETS.reply_enc reply
  )
  in
  reply.left, reply.right, reply.lp

let print_pairs config =
  let ctxt = get_context config in

  let token1 = get_token_by_symbol ctxt "WTON" in
  List.iter (fun token2 ->
      if token1 <> token2 then
        let f token1 token2 =
          let dexpair_address = get_dexpair_address ctxt token1 token2 in
          Printf.printf "Pair %s/%s ( address %s )\n%!"
            token1.token_symbol token2.token_symbol dexpair_address ;
          if not ( contract_exists ctxt dexpair_address ) then
            Printf.printf "   DexPair contract does not exist\n%!"
          else
            let token1_wallet_address =
              get_token_wallet_address ctxt token1 dexpair_address in
            let token2_wallet_address =
              get_token_wallet_address ctxt token2 dexpair_address in
(*
          print_wallet ctxt
            ~address:dexpair_address
            ~wallet_address:token1_wallet_address
            ~token:token1 ;
          print_wallet ctxt
            ~address:dexpair_address
            ~wallet_address:token2_wallet_address
            ~token:token2 ;
*)
            let g token1 token2 =
              let expected_amount, expected_fee =
                get_dexpair_exchange_rate ctxt dexpair_address token1 in
              Printf.printf "   For %s => %s (fee %s)\n%!"
                ( string_of_amount_token "1000000000" token1 )
                ( string_of_amount_token expected_amount token2 )
                expected_fee ;
            in
            g token1 token2 ;
            g token2 token1 ;
            let left_vault_address, right_vault_address =
              get_dexpair_vaults ctxt dexpair_address in
            let _left_wallet_address, right_wallet_address, _lp =
              get_dexpair_wallets ctxt dexpair_address in

            print_wallet ctxt
              ~address:( Printf.sprintf "liquidity %s" token1.token_symbol )
              ~wallet_address:
                (if right_wallet_address = token1_wallet_address then
                   right_vault_address
                 else
                   left_vault_address )
              ~token:token1 ;
            print_wallet ctxt
              ~address:( Printf.sprintf "liquidity %s" token2.token_symbol )
              ~wallet_address:
                (if right_wallet_address = token2_wallet_address then
                   right_vault_address
                 else
                   left_vault_address )
              ~token:token2 ;
        in
        f token1 token2 ;
        (*        f token2 token1 ;*)
        Printf.printf "----------------------------------------------\n%!"
    ) ctxt.manifest.Types.MANIFEST.tokens

*)

open CommandDuneTypes

include Int64.OP

let swap_of_line line =
  if Misc.verbose 2 then
    Printf.eprintf "line: %s\n%!" line;
  let cols = EzString.split line '|' |> Array.of_list in
  let cols = Array.map String.trim cols in
  {
    swap_id = int_of_string cols.(0) ;
    swap_dun_addr = cols.(1) ;
    swap_dun_amount = Int64.of_string cols.(2) ;
    swap_ton_amount = Int64.of_string cols.(3) ;
    swap_ton_pubkey = cols.(10) ;
    swap_ton_addr = cols.(11) ;
  }

let string_of_swap s =
  String.concat "\n" [
    "{";
    Printf.sprintf "  swap_id = %d" s.swap_id ;
    Printf.sprintf "  swap_dun_addr = %s" s.swap_dun_addr ;
    Printf.sprintf "  swap_dun_amount = %Ld" s.swap_dun_amount ;
    Printf.sprintf "  swap_ton_amount = %Ld" s.swap_ton_amount ;
    Printf.sprintf "  swap_ton_pubkey = %s" s.swap_ton_pubkey ;
    Printf.sprintf "  swap_ton_addr = %s" s.swap_ton_addr ;
    "}"
  ]

let action ~swaps_file ~batch_files =

  let done_transfers = Hashtbl.create 1111 in
  let batch_counter = ref 1 in
  List.iter (fun file ->
      let batch = Misc.read_json_file batch_enc file in
      if batch.batch_done then begin
        batch_counter := max !batch_counter ( batch.batch_id + 1 ) ;
        List.iter (fun tr ->
            let key = ( tr.tr_ton_addr, tr.tr_ton_pubkey ) in
            match Hashtbl.find done_transfers key with
            | exception Not_found ->
                Hashtbl.add done_transfers key (ref tr.tr_ton_amount )
            | r ->
                r := !r ++ tr.tr_ton_amount
          ) batch.batch_transfers
      end
    ) batch_files ;

  let lines = EzFile.read_lines swaps_file in
  let nlines = Array.length lines in
  let nswaps = nlines - 4 in
  Printf.eprintf "%d swaps in file\n%!" nswaps;

  let ntransfers = ref 0 in
  let transfers = Hashtbl.create 111 in
  let transfers_list = ref [] in

  let total_ton = ref 0L in
  let total_dun = ref 0L in


  let tr_origin = ref None in
  let tr_foundation = ref None in
  let tr_functori = ref None in
  let tr_investors = ref None in
  let tr_starchain = ref None in

  let add_swap swap =

    total_dun := !total_dun ++ swap.swap_dun_amount ;
    total_ton := !total_ton ++ swap.swap_ton_amount ;

    let key = (swap.swap_ton_addr, swap.swap_ton_pubkey) in
    match Hashtbl.find transfers key with
    | exception Not_found ->
        let r, tr_name = match swap.swap_ton_addr with
          | "0:18a14db7c45c6dd3680c3419e2baca8142e51b0891d62d1c3fb2f877ca8d6a5f"
            -> Some tr_investors, Some "investors"
          | "0:503fac5d901ae4a14141a30afe63d49b62b18690a8ea57e756f36b9defaf5207"
            -> Some tr_foundation, Some "foundation"
          | "0:63179d60797341de55dde95878cf5ea069f77cbaa5fa27fc8992ca22df6fc066"
            -> Some tr_origin, Some "origin-labs"
          | "0:db3a3b62ed5ae6d05c6dd2df9803f8140b35f94949756689d0b62965e275b94a"
            -> Some tr_functori, Some "functori"
          | "0:6011c9c1250b528a085438cc336d7fc2cf8f8dd232aba465a57d99674e06c224" -> Some tr_starchain, Some "starchain"
          | _ -> None, None
        in
        let tr = {
          tr_id = !ntransfers ;
          tr_name ;
          tr_swaps = [ swap ];
          tr_ton_addr = swap.swap_ton_addr ;
          tr_ton_pubkey = swap.swap_ton_pubkey ;
          tr_ton_amount = swap.swap_ton_amount ;
        } in
        Hashtbl.add transfers key tr;
        begin
          match r with
          | None -> ()
          | Some tr_x -> tr_x := Some tr
        end
    | tr ->
        tr.tr_ton_amount <- tr.tr_ton_amount ++ swap.swap_ton_amount
  in

  for n = 2 to 1 + nswaps do
    let line = lines .(n) in
    let swap = swap_of_line line in

    if Misc.verbose 2 then
      Printf.eprintf "swap[%d] = %s\n" n ( string_of_swap swap );
    add_swap swap
  done;

  add_swap { (* starchain lost their key *)
    swap_id = 100000 ;
    swap_dun_addr = "dn1ZdnB77K4F6drVCpTC5VAmzLrgF8XmYKMs" ;
    swap_dun_amount = 29220044_173586L ;
    swap_ton_amount = 580915_387673956L ;
    swap_ton_addr = "0:6011c9c1250b528a085438cc336d7fc2cf8f8dd232aba465a57d99674e06c224" ;
    swap_ton_pubkey = "a6116df76409b066256657a05c527dc423156f172e68b0174ce1af7edc3a552f" ;
  };

  add_swap { (* Alex also lost is key *)
    swap_id = 100001 ;
    swap_dun_addr = "KT1NjjzAH4tLhTRSMDswk2bZpz4tYtHZpfqb" ;
    swap_dun_amount = 75146_073277L ;
    swap_ton_amount = 1493_957719224L ;
    swap_ton_addr = "0:dc27bc25cb47590c804ad20357d73c793527ff68bef18b8de3cd0c2d91ef2327";
    swap_ton_pubkey = "89366e400fe8381dbd33fcf3feb497ea4869e0bdf06631b1a2110fafa4526836";
  } ;

  let _tr_origin = Option.get !tr_origin in
  let _tr_starchain = Option.get !tr_starchain in
  let _tr_functori = Option.get !tr_functori in
  let _tr_foundation = Option.get !tr_foundation in
  let _tr_investors = Option.get !tr_investors in

  Hashtbl.iter (fun key r ->
      match Hashtbl.find transfers key with
      | exception Not_found ->
          Printf.eprintf "Unknown batch to %s %s\n%!"
            (fst key) (snd key) ;
          assert false
      | tr ->
          tr.tr_ton_amount <- tr.tr_ton_amount -- !r;
          if tr.tr_ton_amount = 0L then
            Hashtbl.remove transfers key
    ) done_transfers;

  Hashtbl.iter (fun _key tr ->
      transfers_list := tr :: !transfers_list ;
      incr ntransfers
    ) transfers;


  Printf.eprintf "%d transfers\n%!" !ntransfers;
  Printf.eprintf "total: %s TON\n" (Misc.string_of_nanoton !total_ton);
  Printf.eprintf "total: %Ld DUN\n" (!total_dun // 1_000_000L);

  let transfers_list = List.sort (fun t1 t2 ->
      compare t1.tr_ton_amount t2.tr_ton_amount) !transfers_list in

  let total_team = ref 0L in
  let remaining_transfers = ref [] in
  List.iter (fun tr ->
      match tr.tr_name with
      | None ->
          remaining_transfers := tr :: !remaining_transfers
      | Some name ->
          Printf.eprintf "%s -> %s : %s\n%!"
            name
            tr.tr_ton_addr
            ( Misc.string_of_nanoton tr.tr_ton_amount ) ;
          total_team := !total_team ++ tr.tr_ton_amount
    ) transfers_list;
  Printf.eprintf "total Team: %s TON\n" (Misc.string_of_nanoton !total_team);
  Printf.eprintf "total community: %s TON\n"
    (Misc.string_of_nanoton (!total_ton -- !total_team));

  let remaining_transfers = Array.of_list !remaining_transfers in

  let max_amount x = 100L ** ( x // 111L ) in

  let discarded = ref [] in
  (* Generate next batch *)
  let rec iter amount transfers ntransfers =
    if ntransfers = 0 then
      amount, transfers
    else
      let n = Random.int ntransfers in
      let tr = remaining_transfers.(n) in
      remaining_transfers.(n) <- remaining_transfers.(ntransfers-1);
      let ntransfers = ntransfers-1 in
      if tr.tr_ton_amount > amount then begin
        discarded := tr :: !discarded ;
        iter amount transfers ntransfers
      end else
        iter ( amount -- tr.tr_ton_amount )
          ( tr :: transfers ) ntransfers
  in
  let remaining_amount, transfers =
    iter ( max_amount 1_000_000_000_000_000L )
      [] ( Array.length remaining_transfers ) in
  Printf.eprintf "Discarded %d transfers\n%!" (List.length !discarded);
  Printf.eprintf "Remaining for batch after %d transfers: %s TON\n%!"
    ( List.length transfers )
    ( Misc.string_of_nanoton remaining_amount );

  let total_amount = List.fold_left (fun amount tr ->
      amount ++ tr.tr_ton_amount
    ) 0L transfers in

  match transfers with
  | [] ->
      Error.raise "Not enough transfers for a batch"
  | _ ->
      let batch = {
        batch_id = !batch_counter ;
        batch_transfers = transfers ;
        batch_amount = total_amount ;
        batch_done = false ;
      } in
      let filename = Printf.sprintf "batch-%d.json" batch.batch_id in
      Misc.write_json_file batch_enc filename batch;
      Printf.eprintf "Batch written in %s\n%!" filename;
      ()

let cmd =
  let files = ref [] in
  Misc.cmd
    ["dune"; "airdrop"]
    (fun () ->
       (*
       let config = Config.config () in
       match !args with
       | [] -> print_pairs config
       | args ->
           List.iter ( print_wallets config ) args
*)
       match !files with
       | [] -> Error.raise "You must specify the swaps file"
       | swaps_file :: batch_files ->
           action ~swaps_file ~batch_files
    )
    ~args:
      [
        [], Arg.Anons (fun s -> files := s),
        EZCMD.info ~docv:"FILES" "Swap files and previous batch files";
      ]
    ~doc: "Perform a dune airdrop"
    ~man:[
    ]
