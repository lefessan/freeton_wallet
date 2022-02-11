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

open Solidity_common
open Solidity_ast
open Solidity_checker_TYPES
open Solidity_exceptions

let error = type_error

open Solidity_primitives.UTILS

let rec list_sub n list =
  if n = 0 then [] else
    match list with
    | [] -> failwith "List.sub"
    | x :: tail ->
        x :: ( list_sub (n-1) tail )

let make_surcharged_fun ~nreq pos expected_args opt result =
  match opt.call_args with
  | None -> assert false (* TODO *)
  | Some (AList list) ->
      let len = List.length list in
      if len < nreq then
        error pos "Not enough arguments"
      else
      if len > List.length expected_args then
        error pos "Too many arguments"
      else
        Some
          ( make_fun (List.map (fun (_, type_, _optiona) ->
                type_) ( list_sub len expected_args )) result
                MNonPayable )
  | Some (ANamed list) ->
      let expected_args =
        List.mapi (fun i (name, type_, optional) ->
            name, (i, type_, optional, ref false) )
          expected_args
      in
      let nargs = List.length list in
      let map = EzCompat.StringMap.of_list expected_args in
      List.iter (fun (name, _) ->
          match EzCompat.StringMap.find (Ident.to_string name) map with
          | exception Not_found ->
              error pos "Unknown field %S" (Ident.to_string name)
          | (_pos, _expected_type, _optional, found) ->
              found := true
        ) list ;
      let rec iter args n =
        if n = 0 then
          []
        else
          match args with
          | [] -> assert false
          | ( name, (_i, type_, optional, found) ) :: args ->
              if !found then
                ( type_, Some ( Ident.of_string name ) ) ::
                iter args (n-1)
              else
              if optional then
                iter args n
              else
                error pos "Missing argument %S" name
      in
      let expected_args = iter expected_args nargs in
      Some ( primitive_fun_named expected_args result MNonPayable )


let register_primitives () =
  (* Error handling *)

  register 1
    { prim_name = "assert";
      prim_kind = PrimFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None ->
           Some (make_fun [TBool] [] MPure)
       | _ -> None);

  register 2
    { prim_name = "require";
      prim_kind = PrimFunction }
    (fun _pos opt t_opt ->
       match t_opt, opt.call_args with
       | None, Some ((AList [_] | ANamed [_])) ->
           Some (make_fun [TBool] [] MPure)
       | None, Some ((AList [_;_] | ANamed [_;_])) ->
           if !for_freeton then
             Some (make_fun [TBool; TUint 256] [] MPure)
           else
             Some (make_fun [TBool; TString LMemory] [] MPure)
       | _ -> None);

  register 3
    { prim_name = "revert";
      prim_kind = PrimFunction }
    (fun _pos opt t_opt ->
       match t_opt, opt.call_args with
       | None, Some ((AList [] | ANamed [])) ->
           Some (make_fun [] [] MPure)
       | None, Some ((AList [_] | ANamed [_])) ->
           Some (make_fun [TUint 256] [] MPure)
       | None, Some ((AList [_; _] | ANamed [_ ; _])) ->
           Some (make_fun [TUint 256 ; TString LMemory] [] MPure)
       | _ -> None);

  (* Block/transaction properties *)

  register 4
    { prim_name = "blockhash";
      prim_kind = PrimFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None ->
           Some (make_fun [TUint 256] [TFixBytes 32] MView)
       | _ -> None);

  register 5
    { prim_name = "gasleft";
      prim_kind = PrimFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None ->
           Some (make_fun [] [TUint 256] MView)
       | _ -> None);

  register 6
    { prim_name = "block";
      prim_kind = PrimVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None -> Some (make_var (TMagic (TBlock)))
       | _ -> None);

  register 7
    { prim_name = "coinbase";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TBlock)) -> Some (make_var (TAddress (true)))
       | _ -> None);

  register 8
    { prim_name = "difficulty";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TBlock)) -> Some (make_var (TUint 256))
       | _ -> None);

  register 9
    { prim_name = "gaslimit";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TBlock)) -> Some (make_var (TUint 256))
       | _ -> None);

  register 10
    { prim_name = "number";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TBlock)) -> Some (make_var (TUint 256))
       | _ -> None);

  register 11
    { prim_name = "timestamp";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TBlock)) -> Some (make_var (TUint 256))
       | _ -> None);

  register 12
    { prim_name = "msg";
      prim_kind = PrimVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None -> Some (make_var (TMagic (TMsg)))
       | _ -> None);

  register 13
    { prim_name = "data";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TMsg)) -> Some (make_var (TBytes (LCalldata)))
       | _ -> None);

  register 14
    { prim_name = "sender";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TMsg)) -> Some (make_var (TAddress (true)))
       | _ -> None);

  register 15
    { prim_name = "sig";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TMsg)) -> Some (make_var (TFixBytes 4))
       | _ -> None);

  register 16
    { prim_name = "value";
      prim_kind = PrimMemberVariable }
    (fun pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TMsg)) when !for_freeton ->
           Some (make_var (TUint 128))
       | Some (TMagic (TMsg)) ->
           Some (make_var (TUint 256))
       | Some (TFunction (fd, _fo)) when is_external fd.function_visibility ->
           error pos "Using \".value(...)\" is deprecated. \
                      Use \"{value: ...}\" instead"
       | Some (TAddress _) when !for_freeton ->
           Some (make_var (TUint 256))
       | _ -> None);

  register 17
    { prim_name = "tx";
      prim_kind = PrimVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None -> Some (make_var (TMagic (TTx)))
       | _ -> None);

  register 18
    { prim_name = "gasprice";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TTx)) -> Some (make_var (TUint 256))
       | _ -> None);

  register 19
    { prim_name = "origin";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TTx)) -> Some (make_var (TAddress (true)))
       | _ -> None);

  (* ABI encoding/decoding *)

  register 20
    { prim_name = "abi";
      prim_kind = PrimVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None -> Some (make_var (TMagic (TAbi)))
       | _ -> None);

  register 21
    { prim_name = "decode";
      prim_kind = PrimMemberFunction }
    (fun pos opt t_opt ->
       match t_opt with
       | Some (TMagic (TAbi)) ->
           let atl, rtl =
             match make_prim_args pos opt with
             | None -> [], []
             | Some ([TBytes (LMemory|LCalldata); rt] as atl) ->
                 let rtl =
                   match rt with
                   | TTuple (rtl) -> rtl
                   | _ -> [Some (rt)]
                 in
                 let rtl =
                   List.map (function
                       | Some (TType (t)) ->
                           t
                       | Some (_t) ->
                           error pos "The second argument to abi.decode \
                                      has to be a tuple of types"
                       | None ->
                           error pos "Tuple component can not be empty"
                     ) rtl
                 in
                 atl, rtl
             | Some ([t1; _]) ->
                 error pos "The first argument to abi.decode must be \
                            implicitly convertible to bytes memory \
                            or bytes calldata, but is of type %s"
                   (Solidity_type_printer.string_of_type t1)
             | Some (atl) ->
                 error pos "This function takes two arguments, \
                            but %d were provided" (List.length atl)
           in
           Some (make_fun atl rtl MPure)

       | Some ( TAbstract TvmSlice ) when !for_freeton ->
           begin
             match opt.call_args with
             | Some ( AList list ) ->
                 let res =
                   List.mapi (fun i arg ->
                       match arg with
                       | TType type_ -> type_
                       | _ ->
                           error pos
                             "wrong argument %d, should be a type" i
                     )
                     list
                 in

                 Some (make_fun list res MNonPayable)
             | _ ->
                 Printf.eprintf "wrong args (2) \n%!";
                 None
           end
       | _ -> None);

  register 22
    { prim_name = "encode";
      prim_kind = PrimMemberFunction }
    (fun pos opt t_opt ->
       match t_opt with
       | Some (TMagic (TAbi)) ->
           let atl = preprocess_arg_0 pos (make_prim_args pos opt) in
           Some (make_fun atl [TBytes LMemory] MPure)
       | _ -> None);

  register 23
    { prim_name = "encodePacked";
      prim_kind = PrimMemberFunction }
    (fun pos opt t_opt ->
       match t_opt with
       | Some (TMagic (TAbi)) ->
           let atl = preprocess_arg_0 pos (make_prim_args pos opt) in
           Some (make_fun atl [TBytes LMemory] MPure)
       | _ -> None);

  register 24
    { prim_name = "encodeWithSelector";
      prim_kind = PrimMemberFunction }
    (fun pos opt t_opt ->
       match t_opt with
       | Some (TMagic (TAbi)) ->
           let atl = preprocess_arg_1 pos (TFixBytes 4)
               (make_prim_args pos opt) in
           Some (make_fun atl [TBytes LMemory] MPure)
       | _ -> None);

  register 25
    { prim_name = "encodeWithSignature";
      prim_kind = PrimMemberFunction }
    (fun pos opt t_opt ->
       match t_opt with
       | Some (TMagic (TAbi)) ->
           let atl = preprocess_arg_1 pos (TString (LMemory))
               (make_prim_args pos opt) in
           Some (make_fun atl [TBytes LMemory] MPure)
       | _ -> None);


  (* Mathematical/cryptographic functions *)

  register 26
    { prim_name = "addmod";
      prim_kind = PrimFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None ->
           Some (make_fun [TUint 256; TUint 256; TUint 256] [TUint 256] MPure)
       | _ -> None);

  register 27
    { prim_name = "mulmod";
      prim_kind = PrimFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None ->
           Some (make_fun [TUint 256; TUint 256; TUint 256] [TUint 256] MPure)
       | _ -> None);

  register 28
    { prim_name = "keccak256";
      prim_kind = PrimFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None ->
           Some (make_fun [TBytes LMemory] [TFixBytes 32] MPure)
       | _ -> None);

  register 29
    { prim_name = "sha256";
      prim_kind = PrimFunction }
    (fun _pos opt t_opt ->
       match t_opt with
       | None ->
           begin
             match opt.call_args with
             | Some (AList [ TAbstract TvmSlice ]) ->
                 Some (make_fun [TAbstract TvmSlice] [TUint 256] MPure)
             | _ ->
                 Some (make_fun [TBytes LMemory] [TFixBytes 32] MPure)
           end
       | _ -> None);

  register 30
    { prim_name = "ripemd160";
      prim_kind = PrimFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None ->
           Some (make_fun [TBytes LMemory] [TFixBytes 20] MPure)
       | _ -> None);

  register 31
    { prim_name = "ecrecover";
      prim_kind = PrimFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None ->
           Some (make_fun
                   [TFixBytes 32; TUint 8; TFixBytes 32; TFixBytes 32]
                   [TAddress (false)] MPure)
       | _ -> None);

  (* Contract related *)

  register 32
    { prim_name = "this";
      prim_kind = PrimVariable }
    (fun _pos opt t_opt ->
       match t_opt, opt.current_contract with
       | None, Some (c) ->
           Some (make_var (TContract (
               c.contract_abs_name, c, false (* super *))))
       | _ ->
           None);

  register 33
    { prim_name = "super";
      prim_kind = PrimVariable }
    (fun _pos opt t_opt ->
       match t_opt, opt.current_contract with
       | None, Some (c) ->
           Some (make_var (TContract (
               c.contract_abs_name, c, true (* super *))))
       | _ ->
           None);

  register 34
    { prim_name = "selfdestruct";
      prim_kind = PrimFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None ->
           Some (make_fun [TAddress (true)] [] MNonPayable)
       | _ -> None);

  (* Members of address type *)

  register 35
    { prim_name = "balance";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAddress (_)) when !for_freeton ->
           Some (make_var (TUint 128))
       | Some (TAddress (_)) ->
           Some (make_var (TUint 256))
       | _ -> None);

  register 36
    { prim_name = "transfer";
      prim_kind = PrimMemberFunction }
    (fun pos opt t_opt ->
       match t_opt with
       | Some (TAddress _) when !for_freeton ->
           make_surcharged_fun ~nreq:1 pos
             [ "value", TUint 128, false ;
               "bounce", TBool, true ;
               "flag", TUint 16, true ;
               "body", TAbstract TvmCell, true ;
               "currencies",
               TMapping (TUint 32, TUint 256, LStorage false),
               true;
               "stateInit", TAbstract TvmCell, true;
             ] opt []
       | Some (TAddress (true)) ->
           Some (make_fun [TUint 256] [] MNonPayable)
       | Some (TAddress (false)) ->
           error pos "\"send\" and \"transfer\" are only available \
                      for objects of type \"address payable\", \
                      not \"address\""
       | _ -> None);

  register 37
    { prim_name = "send";
      prim_kind = PrimMemberFunction }
    (fun pos _opt t_opt ->
       match t_opt with
       | Some (TAddress (true)) ->
           Some (make_fun [TUint 256] [TBool] MNonPayable)
       | Some (TAddress (false)) when !for_freeton ->
           Some (make_fun [TUint 256] [TBool] MNonPayable)
       | Some (TAddress (false)) ->
           error pos "\"send\" and \"transfer\" are only available \
                      for objects of type \"address payable\", \
                      not \"address\""
       | _ -> None);

  register 38
    { prim_name = "call";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAddress (_)) ->
           Some (make_fun [TBytes (LMemory)] [TBool; TBytes (LMemory)] MPayable)
       | _ -> None);

  register 39
    { prim_name = "delegatecall";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAddress (_)) ->
           Some (make_fun
                   [TBytes (LMemory)] [TBool; TBytes (LMemory)] MNonPayable)
       | _ -> None);

  register 40
    { prim_name = "staticcall";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAddress (_)) ->
           Some (make_fun [TBytes (LMemory)] [TBool; TBytes (LMemory)] MView)
       | _ -> None);

  (* Type information (members of type) *)

  register 41
    { prim_name = "type";
      prim_kind = PrimFunction }
    (fun pos opt t_opt ->
       match t_opt, make_prim_args pos opt with
       | None, None ->
           Some (make_fun [] [] MPure)
       | None, Some ([TType ((TInt _ | TUint _ | TContract _) as t)]) ->
           Some (make_fun [TType (t)] [TMagic (TMetaType (t))] MPure)
       | None, Some (_) ->
           Some (make_fun [TType (TTuple [])] [] MPure)
       | _ -> None);

  register 42
    { prim_name = "name";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TMetaType (TContract (_, _, _)))) ->
           Some (make_var (TString (LMemory)))
       | _ -> None);

  register 43
    { prim_name = "creationCode";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TMetaType (TContract (_, _, _)))) ->
           Some (make_var (TBytes (LMemory)))
       | _ -> None);

  register 44
    { prim_name = "runtimeCode";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TMetaType (TContract (_, _, _)))) ->
           Some (make_var (TBytes (LMemory)))
       | _ -> None);

  register 45
    { prim_name = "interfaceId";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic (TMetaType (TContract (_, _, _)))) ->
           Some (make_var (TFixBytes (4)))
       | _ -> None);

  let infer_int_type pos list =
    let signed = List.exists (function | TInt _ -> true
                                       | TRationalConst (q, _ ) ->
                                           ExtQ.is_neg q
                                       | _ -> false) list
    in
    let length = ref 0 in
    let bad = ref false in
    List.iter (function
        | TUint n ->
            length := max !length
                ( if signed then n+1 else n)
        | TInt n -> length := max n !length
        | TRationalConst (q, _) ->
            let nbits = Z.numbits (Q.num q) in
            length := max !length
                ( if signed then
                    if ExtQ.is_neg q then
                      nbits
                    else
                      1 + nbits
                  else
                    nbits)
        | t ->
            error pos "non integer type %s\n%!"
              (Solidity_type_printer.string_of_type t);
      ) list ;
    if !bad then
      None
    else
      let t =
        if signed then
          TInt !length
        else
          TUint !length
      in
      Some t
  in

  register 46
    { prim_name = "min";
      prim_kind = PrimMemberVariable }
    (fun pos opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, to_, _loc )) when !for_freeton ->
           Some (make_fun [ ]
                   [ TOptional (TTuple [ Some from_ ;
                                         Some to_ ] ) ] MNonPayable)
       | Some (TMagic (TMetaType (TInt (_) | TUint (_) as t))) ->
           Some (make_var (t))
       | Some (TMagic TMath) when !for_freeton ->
           begin
             match opt.call_args with
             | Some ( AList [] ) -> None
             | Some ( AList list ) ->
                 begin
                   match infer_int_type pos list with
                   | None -> None
                   | Some t ->
                       Some (make_fun
                               ( List.map (fun _ -> t) list )
                               [ t ] MNonPayable )
                 end
             | _ -> None
           end
       | _ -> None);


  register 47
    { prim_name = "max";
      prim_kind = PrimMemberVariable }
    (fun pos opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, to_, _loc )) when !for_freeton ->
           Some (make_fun [ ]
                   [ TOptional (TTuple [ Some from_ ;
                                         Some to_ ] ) ] MNonPayable)
       | Some (TMagic (TMetaType (TInt (_) | TUint (_) as t))) ->
           Some (make_var (t))
       | Some (TMagic TMath) when !for_freeton ->
           begin
             match opt.call_args with
             | Some ( AList [] ) -> None
             | Some ( AList list ) ->
                 begin
                   match infer_int_type pos list with
                   | None -> None
                   | Some t ->
                       Some (make_fun
                               ( List.map (fun _ -> t) list )
                               [ t ] MNonPayable )
                 end
             | _ -> None
           end
       | _ -> None);

  (* Members of array type *)

  register 48
    { prim_name = "length";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TArray (_) | TBytes (_)) ->
           Some (make_var (TUint 256))
       | Some (TFixBytes (_)) ->
           Some (make_var (TUint 8))
       | _ -> None);

  register 49
    { prim_name = "push";
      prim_kind = PrimMemberFunction }
    (fun _pos opt t_opt ->
       match t_opt, opt.call_args with
       | Some (TArray (t, None, (LStorage _))),
         (None | Some (AList [] | ANamed [])) ->
           (* Note: since push only works on storage arrays,
              the argument has a location of storage ref *)
           let t =
             Solidity_type.change_type_location (LStorage false) t in
           Some (make_fun ~returns_lvalue:true [] [t] MNonPayable)
       | Some (TArray (t, None, (LStorage _))),
         Some (_) ->
           (* Note: since push only works on storage arrays,
              the argument has a location of storage ref *)
           let t =
             Solidity_type.change_type_location (LStorage false) t in
           Some (make_fun [t] [] MNonPayable)
       | Some (TArray (t, _, _)), _ when !for_freeton ->
           Some (make_fun [t] [] MNonPayable)
       | Some (TBytes (LStorage _)),
         (None | Some (AList [] | ANamed [])) ->
           Some (make_fun ~returns_lvalue:true [] [TFixBytes (1)] MNonPayable)
       | Some (TBytes (LStorage _)),
         Some (_) ->
           Some (make_fun [TFixBytes (1)] [] MNonPayable)
       | _ -> None);

  register 50
    { prim_name = "pop";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TArray (_, _, LMemory) | TBytes LMemory) when !for_freeton ->
           Some (make_fun [] [] MNonPayable)
       | Some (TArray (_, None, (LStorage _)) | TBytes (LStorage _)) ->
           Some (make_fun [] [] MNonPayable)
       | _ -> None);

  (* Members of function type *)

  register 51
    { prim_name = "address";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TFunction (fd, _fo)) when is_external fd.function_visibility ->
           Some (make_var (TAddress (false)))
       | _ -> None);

  register 52
    { prim_name = "selector";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TFunction (fd, _fo)) when is_external fd.function_visibility ->
           Some (make_var (TFixBytes (4)))
       | _ -> None);

  register 53
    { prim_name = "gas";
      prim_kind = PrimMemberFunction }
    (fun pos _opt t_opt ->
       match t_opt with
       | Some (TFunction (fd, _fo)) when is_external fd.function_visibility ->
           error pos "Using \".gas(...)\" is deprecated. \
                      Use \"{gas: ...}\" instead"
       | _ -> None);

  (* TODO: allow functions with constant arity ? *)
  register 54
    { prim_name = "store";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAbstract TvmBuilder) when !for_freeton ->
           Some (make_fun [TDots] [] MNonPayable)
       | _ -> None);

  register 55
    { prim_name = "tvm";
      prim_kind = PrimVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None when !for_freeton -> Some (make_var (TMagic (TTvm)))
       | _ -> None);

  register 56
    { prim_name = "toCell";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAbstract TvmBuilder) when !for_freeton ->
           Some (make_fun [] [TAbstract TvmCell] MNonPayable)
       | _ -> None);

  register 57
    { prim_name = "hash";
      prim_kind = PrimMemberFunction }
    (fun _pos opt t_opt ->
       match t_opt with
       | Some (TMagic TTvm) when !for_freeton ->
           begin
             match opt.call_args with
             | None -> None
             | Some (AList [
                 TAbstract TvmCell
               | TString _
               | TBytes _
               | TAbstract TvmSlice
               ])
               ->
                 Some (make_fun [TAny] [TUint 256] MNonPayable)
             | _ -> None
           end
       | _ -> None);

  register 58
    { prim_name = "now";
      prim_kind = PrimVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None when !for_freeton ->
           Some (make_var (TUint 32))
       | _ -> None);

  register 59
    { prim_name = "fetch";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, to_, _loc )) when !for_freeton ->
           Some (make_fun [ from_ ] [ TOptional to_ ] MNonPayable)
       | _ -> None);

  register 60
    { prim_name = "hasValue";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TOptional _) when !for_freeton ->
           Some (make_fun [] [ TBool ] MNonPayable)
       | _ -> None);

  register 61
    { prim_name = "get";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TOptional to_) when !for_freeton ->
           Some (make_fun [] [ to_ ] MNonPayable)
       | _ -> None);

  register 62
    { prim_name = "accept";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic TTvm) when !for_freeton ->
           Some (make_fun [] [] MNonPayable)
       | _ -> None);

  register 63
    { prim_name = "pubkey";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic ( TTvm | TMsg )) when !for_freeton ->
           Some (make_fun [] [TUint 256] MNonPayable)
       | _ -> None);

  register 64
    { prim_name = "next";
      prim_kind = PrimMemberFunction }
    (fun _pos opt t_opt ->
       match t_opt, opt.call_args with
       | Some (TMapping ( from_, to_, _loc )), _ when !for_freeton ->
           Some (make_fun [ from_ ]
                   [ TOptional (TTuple [ Some from_ ;
                                         Some to_ ] ) ] MNonPayable)
       | Some (TMagic TRnd), Some (AList [(TInt _| TUint _) as ty]) ->
           Some (make_fun [ty] [ty] MNonPayable)
       | Some (TMagic TRnd), Some (AList []) ->
           Some (make_fun [] [TUint 256] MNonPayable)
       | _ -> None);

  register 65
    { prim_name = "getSeed";
      prim_kind = PrimMemberFunction }
    (fun _pos opt t_opt ->
       match t_opt, opt.call_args with
       | Some (TMagic TRnd), Some (AList []) ->
           Some (make_fun [] [TUint 256] MNonPayable)
       | _ -> None);

  register 66
    { prim_name = "shuffle";
      prim_kind = PrimMemberFunction }
    (fun _pos opt t_opt ->
       match t_opt, opt.call_args with
       | Some (TMagic TRnd), Some (AList [TUint _]) ->
           Some (make_fun [TUint 256] [] MNonPayable)
       | Some (TMagic TRnd), Some (AList []) ->
           Some (make_fun [] [] MNonPayable)
       | _ -> None);

  register 67
    { prim_name = "toSlice";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some ( TString _ ) when !for_freeton ->
           Some (make_fun [] [ TAbstract TvmSlice ] MNonPayable)
       | Some (TAbstract ( TvmBuilder | TvmCell )) when !for_freeton ->
           Some (make_fun [] [TAbstract TvmSlice] MNonPayable)
       | _ -> None);

  register 68
    { prim_name = "functionId";
      prim_kind = PrimMemberFunction }
    (fun _pos opt t_opt ->
       match t_opt, opt.call_args with
       | Some (TMagic (TTvm)),
         Some (AList [TFunction _ | TContract _ as ty ]) ->
           Some (make_fun [ty] [TUint 32] MNonPayable)
       | _ -> None);

  register 69
    { prim_name = "exists";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, _to, _loc )) when !for_freeton ->
           Some (make_fun [ from_ ] [ TBool ] MNonPayable)
       | _ -> None);

  register 70
    { prim_name = "reset";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some ( TOptional _ ) when !for_freeton ->
           Some (make_fun [] [] MNonPayable)
       | _ -> None);

  register 71
    { prim_name = "storeRef";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAbstract TvmBuilder) when !for_freeton ->
           Some (make_fun [TDots] [] MNonPayable)
       | _ -> None);

  register 72
    { prim_name = "append";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TString loc | TBytes loc) when !for_freeton ->
           Some (make_fun [TString loc] [] MNonPayable)
       | _ -> None);

  register 73
    { prim_name = "vergrth16";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some ( TMagic TTvm ) when !for_freeton ->
           Some (make_fun [ TString LMemory ] [ TBool ] MNonPayable)
       | _ -> None);

  register 74
    { prim_name = "buildStateInit";
      prim_kind = PrimMemberFunction }
    (fun pos opt t_opt ->
       match t_opt with
       | Some ( TMagic TTvm ) when !for_freeton ->
           make_surcharged_fun ~nreq:1 pos
             [
               "code", TAbstract TvmCell, false ;
               "data", TAbstract TvmCell, true ;
               "splitDepth", TUint 8, true ;
               "pubkey", TUint 256, true ;
               "contr", TDots, true ; (* TODO do better *)
               "varInit", TDots, true ; (* TODO do better *)
             ] opt
             [ TAbstract TvmCell ]
       | _ -> None);

  register 75
    { prim_name = "commit";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some ( TMagic TTvm ) when !for_freeton ->
           Some (make_fun [] [] MNonPayable)
       | _ -> None);

  register 76
    { prim_name = "setcode";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some ( TMagic TTvm ) when !for_freeton ->
           Some (make_fun [TAbstract TvmCell] [] MNonPayable)
       | _ -> None);

  register 77
    { prim_name = "setCurrentCode";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some ( TMagic TTvm ) when !for_freeton ->
           Some (make_fun [TAbstract TvmCell] [] MNonPayable)
       | _ -> None);

  register 78
    { prim_name = "resetStorage";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some ( TMagic TTvm ) when !for_freeton ->
           Some (make_fun [] [] MNonPayable)
       | _ -> None);

  register 79
    { prim_name = "makeAddrStd";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some ( TType (TAddress _ ) ) when !for_freeton ->
           Some (make_fun [ TInt 8 ; TUint 256 ] [ TAddress true ] MNonPayable)
       | _ ->
           None);

  register 80
    { prim_name = "loadRef";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAbstract TvmSlice) when !for_freeton ->
           Some (make_fun [] [TAbstract TvmCell] MNonPayable)
       | _ -> None);

  register 81
    { prim_name = "format";
      prim_kind = PrimFunction }
    (fun _pos opt t_opt ->
       match t_opt with
       | None when !for_freeton ->
           begin
             match opt.call_args with
             | Some ( AList list ) ->
                 Some (make_fun
                         ( List.mapi (fun i _arg ->
                               if i = 0 then
                                 TString LMemory
                               else
                                 TAny
                             ) list
                         )
                         [TString LMemory ] MNonPayable)

             | _ ->
                 Some (make_fun [ TString LMemory ; TDots ]
                         [TString LMemory ] MNonPayable)
           end
       | _ -> None);

  let register_string n s from_ to_ =
    register n
      { prim_name = s;
        prim_kind = PrimMemberFunction }
      (fun _pos _opt t_opt ->
         match t_opt with
         | Some (TString _) when !for_freeton ->
             Some (make_fun from_ to_ MNonPayable)
         | _ -> None)
  in
  register_string 82 "byteLength" [] [TUint 256] ;

  register 83
    { prim_name = "rawReserve";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic TTvm) when !for_freeton ->
           Some (make_fun [ TUint 256; TUint 8] [] MNonPayable)
       | _ -> None);

  register 84
    { prim_name = "math";
      prim_kind = PrimVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None when !for_freeton -> Some (make_var (TMagic TMath))
       | _ -> None);

  register 85
    { prim_name = "rnd";
      prim_kind = PrimVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None when !for_freeton -> Some (make_var (TMagic TRnd))
       | _ -> None);

  register 86
    { prim_name = "null";
      prim_kind = PrimVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | None when !for_freeton -> Some (make_var (TOptional TAny))
       | _ -> None);

  register 87
    { prim_name = "set";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TOptional to_) when !for_freeton ->
           Some (make_fun [ to_ ] [] MNonPayable)
       | _ -> None);

  register 88
    { prim_name = "wid";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAddress (_)) when !for_freeton ->
           Some (make_var (TInt 8))
       | _ -> None);

  register 89
    { prim_name = "encodeBody";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic TTvm) when !for_freeton ->
           Some (make_fun [ TDots ] [ TAbstract TvmCell ] MNonPayable)
       | _ -> None);

  register 90
    { prim_name = "muldivmod";
      prim_kind = PrimMemberVariable }
    (fun pos opt t_opt ->
       match t_opt with
       | Some (TMagic TMath) ->
           begin
             match opt.call_args with
             | Some ( AList [] ) -> None
             | Some ( AList list ) ->
                 begin
                   match infer_int_type pos list with
                   | None -> None
                   | Some t ->

                       Some (make_fun [ t; t; t] [ t ; t ] MNonPayable )
                 end
             | _ -> None
           end
       | _ -> None);

  register 91
    { prim_name = "deploy";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic TTvm) ->
           Some (make_fun [
               TAbstract TvmCell; (* stateInit *)
               TAbstract TvmCell; (* payload *)
               TUint 128 ;
               TInt 8
             ]
               [ TAddress true ] MNonPayable )
       | _ -> None);

  let muldiv_kind pos opt t_opt =
    match t_opt with
    | Some (TMagic TMath) ->
        begin
          match opt.call_args with
          | Some ( AList [] ) -> None
          | Some ( AList list ) ->
              begin
                match infer_int_type pos list with
                | None -> None
                | Some t ->
                    Some (make_fun [ t; t; t] [ t ] MNonPayable )
              end
          | _ -> None
        end
    | _ -> None
  in
  register 92
    { prim_name = "muldiv";
      prim_kind = PrimMemberFunction } muldiv_kind ;
  register 93
    { prim_name = "muldivr";
      prim_kind = PrimMemberFunction } muldiv_kind ;
  register 94
    { prim_name = "muldivc";
      prim_kind = PrimMemberFunction } muldiv_kind ;

  register 95
    { prim_name = "empty";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMapping ( _from, _to, _loc )) when !for_freeton ->
           Some (make_fun [] [ TBool ] MNonPayable)
       | Some (TArray ( _, _, _ )) when !for_freeton ->
           Some (make_fun [] [ TBool ] MNonPayable)
       | Some (TString _) when !for_freeton ->
           Some (make_fun [] [ TBool ] MNonPayable)
       | Some (TAbstract TvmSlice) when !for_freeton ->
           Some (make_fun [] [ TBool ] MNonPayable)

       | _ -> None);

  register 96
    { prim_name = "stoi";
      prim_kind = PrimFunction }
    (fun _pos _opt _t_opt ->
       Some (make_fun [ TString LMemory ]
               [ TUint 256; TBool ] MNonPayable)
    );

  register 97
    { prim_name = "makeAddrNone";
      prim_kind = PrimMemberFunction }
    (fun pos _opt t_opt ->
       match t_opt with
       | Some ( TType (TAddress _) ) ->
           Some (make_fun [] [TAddress true] MNonPayable)
       | Some t ->
           error pos "address has type %s\n%!"
             (Solidity_type_printer.string_of_type t);
       | None -> None
    );

  register 98
    { prim_name = "storeTons";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAbstract TvmBuilder) when !for_freeton ->
           Some (make_fun [ TUint 128 ] [] MNonPayable)
       | _ -> None);

  register 99
    { prim_name = "storeUnsigned";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAbstract TvmBuilder) when !for_freeton ->
           Some (make_fun [ TUint 256 ; TUint 16 ] [] MNonPayable)
       | _ -> None);

  register 100
    { prim_name = "add";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, to_, _loc )) when !for_freeton ->
           Some (make_fun [ from_  ; to_ ] [] MNonPayable)
       | _ -> None);

  register 101
    { prim_name = "at";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, to_, _loc )) when !for_freeton ->
           (* raise an exception is missing *)
           Some (make_fun [ from_ ] [ to_ ] MNonPayable)
       | _ -> None);

  register 102
    { prim_name = "prev";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, to_, _loc )) when !for_freeton ->
           Some (make_fun [ from_ ]
                   [ TOptional (TTuple [ Some from_ ;
                                         Some to_ ] ) ] MNonPayable)
       | _ -> None);

  register 103
    { prim_name = "nextOrEq";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, to_, _loc )) when !for_freeton ->
           Some (make_fun [ from_ ]
                   [ TOptional (TTuple [ Some from_ ;
                                         Some to_ ] ) ] MNonPayable)
       | _ -> None);

  register 104
    { prim_name = "prevOrEq";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, to_, _loc )) when !for_freeton ->
           Some (make_fun [ from_ ]
                   [ TOptional (TTuple [ Some from_ ;
                                         Some to_ ] ) ] MNonPayable)
       | _ -> None);

  register 105
    { prim_name = "delMin";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, to_, _loc )) when !for_freeton ->
           Some (make_fun []
                   [ TOptional (TTuple [ Some from_ ;
                                         Some to_ ] ) ] MNonPayable)
       | _ -> None);

  register 106
    { prim_name = "delMax";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, to_, _loc )) when !for_freeton ->
           Some (make_fun []
                   [ TOptional (TTuple [ Some from_ ;
                                         Some to_ ] ) ] MNonPayable)
       | _ -> None);

  register 107
    { prim_name = "replace";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, to_, _loc )) when !for_freeton ->
           Some ( make_fun [ from_ ; to_ ] [ TBool ] MNonPayable )
       | _ -> None);

  register 108
    { prim_name = "getSet";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, to_, _loc )) when !for_freeton ->
           Some ( make_fun [ from_ ; to_ ] [ TOptional to_ ] MNonPayable )
       | _ -> None);

  register 109
    { prim_name = "getAdd";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, to_, _loc )) when !for_freeton ->
           Some ( make_fun [ from_ ; to_ ] [ TOptional to_ ] MNonPayable )
       | _ -> None);

  register 110
    { prim_name = "getReplace";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMapping ( from_, to_, _loc )) when !for_freeton ->
           Some ( make_fun [ from_ ; to_ ] [ TOptional to_ ] MNonPayable )
       | _ -> None);

  register 111
    { prim_name = "getType";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAddress _) ->
           Some ( make_fun [] [ TUint 8] MNonPayable )
       | _ -> None);

  register 112
    { prim_name = "isStdZero";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAddress _) ->
           Some ( make_fun [] [ TBool] MNonPayable )
       | _ -> None);

  register 113
    { prim_name = "isExternZero";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAddress _) ->
           Some ( make_fun [] [ TBool] MNonPayable )
       | _ -> None);

  register 114
    { prim_name = "isNone";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAddress _) ->
           Some ( make_fun [] [ TBool] MNonPayable )
       | _ -> None);

  register 115
    { prim_name = "unpack";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAddress _) ->
           Some ( make_fun [] [ TInt 8 ; TUint 256] MNonPayable )
       | _ -> None);

  let tvm_prim num prim_name from_ to_ =
    register num
      { prim_name ;
        prim_kind = PrimMemberFunction }
      (fun _pos _opt t_opt ->
         match t_opt with
         | Some (TMagic TTvm) ->
             Some (make_fun from_ to_ MNonPayable )
         | _ -> None);
  in
  tvm_prim 116 "codeSalt"
    [ TAbstract TvmCell ]
    [ TOptional ( TAbstract TvmCell ) ];
  tvm_prim 117 "rawCommit" [] [];
  tvm_prim 118 "setData" [ TAbstract TvmCell ] [];
  tvm_prim 119 "log" [ TString LMemory ] [];
  tvm_prim 120 "hexdump" [ TAny ] [];
  tvm_prim 121 "bindump" [ TAny ] [];
  tvm_prim 122 "configParam" [ TUint 8 ] [ TDots ];
  tvm_prim 123
    "rawConfigParam" [ TUint 8 ] [ TAbstract TvmCell ; TBool ];

  tvm_prim 124 "sendrawmsg" [TAbstract TvmCell; TUint 8] [TAbstract TvmCell];

  register 125
    { prim_name = "buildExtMsg";
      prim_kind = PrimMemberFunction }
    (fun pos opt t_opt ->
       match t_opt, opt.call_args with
       | Some (TMagic (TTvm)),  Some (ANamed _) ->
           make_surcharged_fun ~nreq:7 pos [
             "callbackId", TAny, false; (* (uint32 | functionIdentifier) *)
             "call", TAny, false;
             "dest", TAddress true, false;
             "expire", TUint 32, true;
             "pubkey", TOptional (TUint 256), true;
             "onErrorId", TAny, false; (* (uint32 | functionIdentifier) *)
             "signBoxHandle", TOptional (TUint 32), true;
             "sign", TBool, true;
             "stateInit", TAbstract TvmCell, false;
             "time", TUint 64, false;
           ] opt [ TAbstract TvmCell ]
       | _ -> None);

  register 126
    { prim_name = "buildIntMsg";
      prim_kind = PrimMemberFunction }
    (fun pos opt t_opt ->
       match t_opt, opt.call_args with
       | Some (TMagic (TTvm)),  Some (ANamed _) ->
           make_surcharged_fun ~nreq:3 pos [
             "call", TAny, false;
             "dest", TAddress true, false;
             "value", TUint 128, false;
             "bounce", TBool, true;
             "stateInit", TAbstract TvmCell, true;
             "currencies",
             TMapping (TUint 32, TUint 256, LStorage false), true;
           ] opt [ TAbstract TvmCell ]
       | _ -> None);

  register 127
    { prim_name = "logtvm";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TMagic TTvm) ->
           Some (make_fun [TString LMemory] [] MView)
       | _ -> None);

  register 128
    { prim_name = "checkSign";
      prim_kind = PrimFunction }
    (fun _pos opt t_opt ->
       match t_opt with
       | Some ( TMagic TTvm ) ->
           begin
             match opt.call_args with
             | Some (AList
                       ((
                         [ TUint 256 ; TUint 256 ; TUint 256 ; TUint 256 ]
                       | [ TUint 256 ; TAbstract TvmSlice ; TUint 256 ]
                       | [ TAbstract TvmSlice ; TAbstract TvmSlice ; TUint 256 ]
                       ) as from_ ) ) ->
                 Some (make_fun from_ [TBool] MPure)
             | _ -> None
           end
       | _ -> None);

  tvm_prim 129 "insertPubkey" [ TAbstract TvmCell ; TUint 256 ]
    [ TAbstract TvmCell ];
  tvm_prim 130 "buildEmptyData" [ TUint 256 ] [ TAbstract TvmCell ];
  tvm_prim 131 "code" [] [ TAbstract TvmCell ];
  tvm_prim 132 "setCodeSalt" [ TAbstract TvmCell; TAbstract TvmCell ] [ TAbstract TvmCell ];
  tvm_prim 133 "setPubkey" [ TUint 256 ] [ ];
  tvm_prim 134 "exit" [ ] [ ];
  tvm_prim 135 "exit1" [ ] [ ];
  (* missing TVM instructions:
      * [tvm.buildExtMsg()](#tvmbuildextmsg)
      * [tvm.buildIntMsg()](#tvmbuildintmsg)
  *)

  register 136
    { prim_name = "bits";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAbstract TvmSlice | TAbstract TvmBuilder) ->
           Some (make_fun [] [TUint 16] MNonPayable)
       | _ -> None);

  register 137
    { prim_name = "loadRefAsSlice";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAbstract TvmSlice) ->
           Some (make_fun [] [TAbstract TvmSlice] MNonPayable)
       | _ -> None);

  register 138
    { prim_name = "decodeFunctionParams";
      prim_kind = PrimMemberFunction }
    (fun _pos opt t_opt ->
       match t_opt, opt.call_args with
       | Some (TAbstract TvmSlice),
         Some (
           AList [TFunction (
               { function_def = Some {fun_responsible = true; _ };
                 function_params; _
               } as desc,
               opt
             )]
         ) ->
           Some (
             make_fun [TFunction (desc, opt)]
               (TUint 32 :: List.map fst function_params)
               MNonPayable
           )
       | Some (TAbstract TvmSlice),
         Some (AList [TFunction ({ function_params; _ } as desc, opt)]) ->
           Some (
             make_fun [TFunction (desc, opt)]
               (List.map fst function_params)
               MNonPayable
           )
       | Some (TAbstract TvmSlice),
         Some (AList [TContract (id, desc, super)]) ->
           Some (make_fun [TContract (id, desc, super)] [TDots] MNonPayable)
       | _ -> None);

  register 139
    { prim_name = "size";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAbstract TvmSlice | TAbstract TvmBuilder) ->
           Some (make_fun [] [TUint 16; TUint 8] MView)
       | _ -> None);

  let div_kind pos opt t_opt =
    match t_opt with
    | Some (TMagic TMath) ->
        begin
          match opt.call_args with
          | Some ( AList [] ) -> None
          | Some ( AList list ) ->
              begin
                match infer_int_type pos list with
                | None -> None
                | Some t ->
                    Some (make_fun [ t; t] [ t ] MNonPayable )
              end
          | _ -> None
        end
    | _ -> None
  in
  register 140
    { prim_name = "divr";
      prim_kind = PrimMemberFunction } div_kind ;
  register 141
    { prim_name = "divc";
      prim_kind = PrimMemberFunction } div_kind ;


  register 142
    { prim_name = "loadUnsigned";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAbstract TvmSlice) ->
           (* return value should directly depend on argument value *)
           Some (make_fun [TUint 16] [TUint 1] MNonPayable)
       | _ -> None);

  (*  register_string 132 "substr" [] [TUint 256] ; *)

  register 143
    { prim_name = "substr";
      prim_kind = PrimMemberFunction }
    (fun _pos opt t_opt ->
       match t_opt with
       | Some (TString _) when !for_freeton ->
           begin
             match opt.call_args with
             | Some ( AList [  _from ] ) ->
                 Some (make_fun [ TUint 256] [ TString LMemory ] MNonPayable )
             | Some ( AList [  _from; _count ] ) ->
                 Some (make_fun [ TUint 256 ; TUint 256 ] [ TString LMemory ] MNonPayable )

             | _ -> None
           end

       | _ -> None);

  let register_string_find n s =
    register n
      { prim_name = s;
        prim_kind = PrimMemberFunction }
      (fun _pos _opt t_opt ->
         match t_opt with
         | Some (TString _) when !for_freeton ->
             Some (make_fun [ TString LMemory ] [ TOptional (TUint 32) ] MNonPayable )
         | _ -> None)
  in
  register_string_find 144 "find";
  register_string_find 145 "findLast";

  register 146
    { prim_name = "extMsg";
      prim_kind = PrimMemberVariable }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAbstract TvmCall) ->
           Some (make_var (TAbstract TvmExtCall))
       | _ -> None );

  register 147
    { prim_name = "div";
      prim_kind = PrimMemberVariable } div_kind ;

  register 148
    { prim_name = "refs";
      prim_kind = PrimMemberFunction }
    (fun _pos _opt t_opt ->
       match t_opt with
       | Some (TAbstract TvmSlice) when !for_freeton ->
           Some (make_fun [] [ TUint 8 ] MNonPayable)
       | _ -> None);

  ()

let handle_exception f x =
  try
    Ok ( f x )
  with
  | (
    Solidity_common.GenericError _
  | Solidity_exceptions.SyntaxError _
  | Solidity_exceptions.TypecheckError _
  )  as exn ->
      Error ( Solidity_exceptions.string_of_exn exn )

let parse_file = Solidity_parser.parse_file ~freeton:true

let parse_files = Solidity_parser.parse_files ~freeton:true

let typecheck_ast =
  Solidity_typechecker.type_program ~init:register_primitives
let string_of_ast = Solidity_printer.string_of_program ~freeton:true

open Solidity_typechecker

(* TODO: use the 'fields' set of fo to detect re-use *)

let type_options_fun opt env pos is_payable fo opts =
  List.fold_left (fun fo (id, e) ->
      let id = strip id in
      let fo, already_set =
        match Ident.to_string id, fo.kind with
        | "value", KExtContractFun when
            not !for_freeton && not is_payable ->
            error pos "Cannot set option \"value\" \
                       on a non-payable function type"
        | "value", KNewContract when
            not !for_freeton && not is_payable ->
            error pos "Cannot set option \"value\", since the \
                       constructor of contract is non-payable"
        | "value", ( KExtContractFun | KNewContract | KReturn ) ->
            expect_expression_type opt env e (TUint 256);
            { fo with value = true }, fo.value
        | "gas", KExtContractFun ->
            expect_expression_type opt env e (TUint 256);
            { fo with gas = true }, fo.gas
        | "salt", KNewContract ->
            expect_expression_type opt env e (TFixBytes 32);
            { fo with salt = true }, fo.salt
        | "gas", KNewContract ->
            error pos "Function call option \"%s\" cannot \
                       be used with \"new\""
              (Ident.to_string id);
        | "salt", KExtContractFun ->
            error pos "Function call option \"%s\" can \
                       only be used with \"new\""
              (Ident.to_string id);
            (* FREETON *)
            (* TODO: check that mandatory fields are provided *)
        | "pubkey", ( KNewContract | KExtContractFun )
          ->
            expect_expression_type opt env e
              ( TOptional (TUint 256));
            fo, false (* TODO *)
        | "code", KNewContract  ->
            expect_expression_type opt env e (TAbstract TvmCell);
            fo, false (* TODO *)
        | "flag", ( KExtContractFun | KNewContract | KReturn )  ->
            expect_expression_type opt env e (TUint 8);
            fo, false (* TODO *)
        | "varInit", KNewContract  ->
            fo, false (* TODO *)
        | "abiVer", KExtContractFun  ->
            expect_expression_type opt env e (TUint 8);
            fo, false (* TODO *)
        | "extMsg", KExtContractFun  ->
            expect_expression_type opt env e TBool ;
            fo, false (* TODO *)
        | "sign", KExtContractFun  ->
            expect_expression_type opt env e TBool ;
            fo, false (* TODO *)
        | "bounce", ( KExtContractFun | KNewContract | KReturn )  ->
            expect_expression_type opt env e TBool ;
            fo, false (* TODO *)
        | "stateInit", KNewContract  ->
            expect_expression_type opt env e (TAbstract TvmCell) ;
            fo, false (* TODO *)
        | "wid", KNewContract  ->
            expect_expression_type opt env e (TInt 8) ;
            fo, false (* TODO *)
        | "time", KExtContractFun  ->
            expect_expression_type opt env e (TUint 64) ;
            fo, false (* TODO *)
        | "expire", KExtContractFun  ->
            expect_expression_type opt env e (TUint 64) ;
            fo, false (* TODO *)
        | "callbackId", KExtContractFun  ->
            expect_expression_type opt env e (TUint 64) ;
            fo, false (* TODO *)
        | "callback", KExtContractFun  ->
            fo, false (* TODO *)
        | "onErrorId", KExtContractFun  ->
            expect_expression_type opt env e (TUint 64) ;
            fo, false (* TODO *)
        | _, KOther ->
            error pos "Function call options can only be set on \
                       external function calls or contract creations"
              (Ident.to_string id);
        | _ ->
            error pos "Unknown option \"%s\". Valid options are \
                       \"salt\", \"value\" and \"gas\""
              (Ident.to_string id);
      in
      if already_set then
        error pos "Option \"%s\" has already been set"
          (Ident.to_string id);
      fo
    ) fo opts

let () =
  Solidity_typechecker.type_options_ref := type_options_fun;
  let list =
    Solidity_raw_parser.[
      "inline", INLINE;
      "static", STATIC;
      "optional", OPTIONAL;
      "onBounce", ONBOUNCE;
      "repeat", REPEAT;
      "responsible", RESPONSIBLE;

      "TvmCell", TYPEABSTRACT "TvmCell";
      "TvmSlice", TYPEABSTRACT "TvmSlice";
      "TvmBuilder", TYPEABSTRACT "TvmBuilder";

      "nano", NUMBERUNIT (Nanoton);
      "nanoton", NUMBERUNIT (Nanoton);
      "nTon", NUMBERUNIT (Nanoton);

      "micro", NUMBERUNIT (Microton);
      "microton", NUMBERUNIT (Microton);

      "milli", NUMBERUNIT (Milliton);
      "milliton", NUMBERUNIT (Milliton);

      "ton", NUMBERUNIT (Ton);
      "Ton", NUMBERUNIT (Ton);

      "kiloton", NUMBERUNIT (Kiloton);
      "kTon", NUMBERUNIT (Kiloton);

      "megaton", NUMBERUNIT (Megaton);
      "MTon", NUMBERUNIT (Megaton);

      "gigaton", NUMBERUNIT (Gigaton);
      "GTon", NUMBERUNIT (Gigaton);

      (* "nano", NUMBERUNIT (Ever); *)
      "nanoever", NUMBERUNIT (Nanoever);
      "nEver", NUMBERUNIT (Nanoever);

      "micro", NUMBERUNIT (Microever);
      "microever", NUMBERUNIT (Microever);

      "milli", NUMBERUNIT (Milliever);
      "milliever", NUMBERUNIT (Milliever);

      "ever", NUMBERUNIT (Ever);
      "Ever", NUMBERUNIT (Ever);

      "kiloever", NUMBERUNIT (Kiloever);
      "kEver", NUMBERUNIT (Kiloever);

      "megaever", NUMBERUNIT (Megaever);
      "MEver", NUMBERUNIT (Megaever);

      "gigaever", NUMBERUNIT (Gigaever);
      "GEver", NUMBERUNIT (Gigaever);
    ]

  in
  Solidity_lexer.init2 ~list ()
