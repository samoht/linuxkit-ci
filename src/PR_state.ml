open Datakit_ci
open Datakit_github
open Lwt.Infix
open Astring

let ( >>*= ) x f =
  x >>= function
  | Ok x    -> f x
  | Error e -> Utils.failf "Unexpected DB error: %a" DK.pp_error e


let ( // ) = Datakit_client.Path.Infix.( / )

type v = [
  | `OkToTest
  | `InWhiteList
  | `NotAuthorized
]

let cached_file = Cache.Path.(value // "state")

let string_of_v = function
  | `OkToTest      -> "ok-to-test"
  | `InWhiteList   -> "in-whitelist"
  | `NotAuthorized -> "not-authorized"

let v_of_string = function
  | "ok-to-test"     -> `OkToTest
  | "in-whitelist"   -> `InWhiteList
  | "not-authorized" -> `NotAuthorized
  | _ -> `NotAuthorized

(* to accept this pull request for testing *)
let ok_to_test = "ok to test"

module Builder = struct
  module Key = struct
    type t = Whitelist.v
  end
  type t = unit
  type value = v
  type context = Datakit_github.PR.t

  let name _ = "LinuxKit PR state"
  let title _ _ = "Computing the PR state"

  let generate () ~switch:_ ~log:_ tr pr whitelist =
    let in_whitelist user = Whitelist.mem (User.name user) whitelist in
    let contains x body =
      let re = Re.compile Re.(seq [rep space; str x; rep space]) in
      Re.execp re body
    in
    let state =
      if in_whitelist (PR.owner pr) then `InWhiteList
      else
        let is_ok_to_test c =
          (* The user doing the comment is in the whitelist and he/she
             says "test this" *)
          in_whitelist (Comment.user c) && contains ok_to_test (Comment.body c)
        in
        let comments = Array.to_list (PR.comments pr) in
        if List.exists is_ok_to_test comments then `OkToTest else `NotAuthorized
    in
    let file = Cstruct.of_string (string_of_v state ^ "\n") in
    DK.Transaction.create_file tr cached_file file >>*= fun () ->
    Lwt.return (Ok state)

  let load () tree _ =
    DK.Tree.read_file tree cached_file >>*= fun file ->
    let state = v_of_string (String.trim @@ Cstruct.to_string file) in
    Lwt.return state

  let branch () whitelist = "state-" ^ Whitelist.hash whitelist

end

module Result_cache = Cache.Make(Builder)

type t = Result_cache.t

let v t whitelist pr =
  let open! Term.Infix in
  Result_cache.find t pr whitelist

let make ~logs = Result_cache.create ~logs ()
