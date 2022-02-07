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

open Ezcmd.V2
open EZCMD.TYPES

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
      if batch.batch_frozen then begin
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
          tr_ready = false ;
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

  let tr_origin = Option.get !tr_origin in
  let tr_starchain = Option.get !tr_starchain in
  let tr_functori = Option.get !tr_functori in
  let tr_foundation = Option.get !tr_foundation in
  let tr_investors = Option.get !tr_investors in

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
  let max_amount = max_amount 1_000_000_000_000_000L in
  let share = max_amount // 4L in
  let community_share = 3L ** share in
  let founders_share = max_amount -- community_share in

  let discarded = ref [] in
  (* Generate next batch *)
  let rec iter amount count transfers ntransfers =
    if count = 94 then
      amount, transfers
    else
    if ntransfers = 0 then
      amount, transfers
    else
      let n = Random.int ntransfers in
      let tr = remaining_transfers.(n) in
      remaining_transfers.(n) <- remaining_transfers.(ntransfers-1);
      let ntransfers = ntransfers-1 in
      if tr.tr_ton_amount > amount then begin
        discarded := tr :: !discarded ;
        iter amount count transfers ntransfers
      end else
        iter ( amount -- tr.tr_ton_amount ) (count+1)
          ( tr :: transfers ) ntransfers
  in
  let remaining_amount, transfers =
    iter community_share 0
      [] ( Array.length remaining_transfers ) in
  Printf.eprintf "Discarded %d transfers\n%!" (List.length !discarded);

  let remaining_amount = remaining_amount ++ founders_share in
  Printf.eprintf "Remaining for batch after %d transfers: %s TON\n%!"
    ( List.length transfers )
    ( Misc.string_of_nanoton remaining_amount );

  let remaining_amount, transfers =
    if remaining_amount > 0L && tr_origin.tr_ton_amount > 0L then begin
      if tr_origin.tr_ton_amount > 150_000_000_000_000L then
         tr_origin.tr_ton_amount <- 150_000_000_000_000L ;
      if remaining_amount >= tr_origin.tr_ton_amount then
        remaining_amount -- tr_origin.tr_ton_amount,
        tr_origin :: transfers
      else
        0L,
        let transfer = { tr_origin with
                         tr_ton_amount = remaining_amount  }
        in
        tr_origin.tr_ton_amount <- tr_origin.tr_ton_amount -- remaining_amount;
        transfer :: transfers
    end
    else
      remaining_amount, transfers
  in

  Printf.eprintf "Remaining before investors: %s\n%!"
    ( Misc.string_of_nanoton remaining_amount );
  let rec iter transfers remaining_amount accounts =
    let len = List.length accounts in
    if remaining_amount = 0L || len = 0 then
      remaining_amount, transfers
    else
      let share = remaining_amount // ( Int64.of_int len ) in
      let rec iter2 transfers remaining_amount accounts kept =
        match accounts with
        | [] ->
            0L,
            ( List.map (fun tr ->
                  tr.tr_ton_amount <- share;
                  tr
                ) kept )
            @ transfers
        | tr :: accounts ->
            if tr.tr_ton_amount = 0L then
              iter transfers remaining_amount
                ( accounts @ kept )
            else
            if tr.tr_ton_amount <= share then
              let transfers = tr :: transfers in
              let remaining_amount = remaining_amount -- tr.tr_ton_amount in
              iter transfers remaining_amount
                ( accounts @ kept )
            else
              iter2 transfers remaining_amount accounts ( tr :: kept )
      in
      iter2 transfers remaining_amount accounts []
  in
  let remaining_amount, transfers = iter transfers remaining_amount
      [
        tr_functori ;
        tr_investors ;
        tr_starchain ;
        tr_foundation ;
      ]
  in
  Printf.eprintf "Final Remaining for batch after %d transfers: %s TON\n%!"
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
        batch_frozen = true ;
        batch_ready = false ;
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
