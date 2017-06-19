open Datakit_ci
open Lwt.Infix
open Astring

let ( // ) = Datakit_client.Path.Infix.( / )
let ( / ) = Filename.concat

let ( >>*= ) x f =
  x >>= function
  | Ok x    -> f x
  | Error e -> Utils.failf "Unexpected DB error: %a" DK.pp_error e

let cached_file = Cache.Path.(value // "whitelist")

type v = string list

module Builder = struct
  module Key = struct
    type t = Git.commit
  end
  type t = unit
  type value = string list
  type context = job_id
  let name _ = "LinuxKit whitelist"
  let title _ _ = "Reading the whitelist"

  let generate () ~switch:_ ~log  tr job_id head =
    Git.with_clone ~log ~job_id head (fun path ->
        let file = path / "whitelist" in
        (if not (Sys.file_exists file) then Lwt.return []
         else
           Lwt_io.open_file ~mode:Lwt_io.Input file >>= fun ic ->
           Lwt_stream.to_list (Lwt_io.read_lines ic)
        ) >>= fun lines ->
        let lines = List.map String.trim lines in
        let file = Cstruct.of_string (String.concat ~sep:"\n" lines ^ "\n") in
        DK.Transaction.create_file tr cached_file file >>*= fun () ->
        Lwt.return (Ok lines)
      )

  let load () tree _head =
    DK.Tree.read_file tree cached_file >>*= fun file ->
    let lines = String.cuts ~empty:false ~sep:"\n" (Cstruct.to_string file) in
    Lwt.return lines

  let branch () head = "whitelist-" ^ Git.hash head

end

module Result_cache = Cache.Make(Builder)

type t = Result_cache.t

let v t src =
  let open! Term.Infix in
  Term.job_id >>= fun job_id ->
  Result_cache.find t job_id src

let mem = List.mem

let make ~logs = Result_cache.create ~logs ()

let hash t =
  let t = List.sort String.compare t in
  let `Hex h = Hex.of_string (String.concat ~sep:" ? " t) in
  h
