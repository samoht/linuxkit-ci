open Astring
open Datakit_ci

(* Check whether we're running in production or locally, as some settings should be changed
   depending on this. *)
let profile =
  match Sys.getenv "PROFILE" with
  | "localhost" -> `Localhost
  | "production" -> `Production
  | x -> Utils.failf "Unknown $PROFILE setting %S" x
  | exception Not_found -> Utils.failf "$PROFILE not set"

(* Used for branch and tag builds. Pool can be enlarged easily if needed. *)
let pool = Monitored_pool.create "LinuxKit builds" 4

(* Used for building PRs, testing GCP images and running LTP tests. *)
let google_pool = Monitored_pool.create "LinuxKit builds on GCP" 4

(* Keeps track of our Google Compute instances. *)
let vms =
  let prefix =
    match profile with
    | `Production -> "linuxkit-ci-"
    | `Localhost -> "staging-linuxkit-ci-"      (* Use distinct names when testing *)
  in
  Gcp.make ~state:"/ci-state/gcloud-vms" ~prefix

let repo project =
  (* Our local Git clone of the source repository. *)
  let remote =
    match profile with
    | `Production -> Fmt.strf "https://github.com/linuxkit/%s.git" project
    | `Localhost  -> "/fake-remote/" ^ project  (* local "remote" *)
  in
  Git.v ~logs ~remote ("/repos/" ^ project)

(* Our local Git clone of the LinuxKit source repository. *)
let src_repo = repo "linuxkit"

(* The repository and branch where the whitelist is stored *)
let whitelist_repo = repo "linuxkit-ci"
let whitelist_ref =
  `Ref (Datakit_github.(Repo.v ~user:(User.v "linuxkit") ~repo:"linuxkit-ci"),
        ["master"])

let ci_repo =
  let remote =
    match profile with
    | `Production -> "https://github.com/linuxkit/linuxkit-ci.git"
    | `Localhost -> "/fake-remote/linuxkit-ci"       (* Pull from our local "remote" *)
  in
  Git.v ~logs ~remote "/repos/linuxkit-ci"

(* Cache of built images. Can be deleted without too much trouble, but e.g. if you try to
   re-run the tests after deleting the image it will fail and you'll have to rebuild the
   image first. *)
let build_cache = Disk_cache.make ~path:"/build-cache"

let docker_pool = Monitored_pool.create "Docker" 4

let minute = 60.

let ci_dockerfile =
  let timeout = 10. *. minute in
  Docker.create ~logs ~pool:docker_pool ~timeout ~label:"CI.Dockerfile" "Dockerfile"

module Builder = struct
  open Term.Infix

  let check_builds term =
    term >|= fun (_:Docker.Image.t) -> "Build succeeded"

  let whitelist = Whitelist.make ~logs
  let pr_state = PR_state.make ~logs
  let builder = Linuxkit_build.make ~logs ~pool ~google_pool ~vms ~build_cache
  let tester = Linuxkit_test.make ~logs ~google_pool ~vms ~build_cache

  (* read the whitelist *)
  let read_whitelist =
    Git.fetch_head whitelist_repo whitelist_ref >>=
    Whitelist.v whitelist

  (* check if the target can be tested *)
  let should_be_tested ~target whitelist =
    Term.target target >>= function
    | `Ref _ -> Term.return true
    | `PR pr ->
      PR_state.v pr_state whitelist pr >|= function
      | `OkToTest
      | `InWhiteList   -> true
      | `NotAuthorized -> false

  (* To build, "git fetch" the head of the branch, tag or PR being
     tested, then use [builder]. *)
  let build ~target =
    Git.fetch_head src_repo target >>=
    Linuxkit_build.build builder ~target

  (* How to test the various images we produce. *)
  let test_images results =
    let get x =
      match String.Map.find x results with
      | Some x -> Term.return x
      | None -> Term.fail "Output %s not found" x
    in
    Term.wait_for_all [
      "GCP", get "test.img.tar.gz" >>=
      Linuxkit_test.gcp tester
    ]
    >|= fun () -> "All tests passed"

  (* The "linuxkit-ci" status for a target is the result of building
     it and then testing the images. *)
  let status target = [
    "linuxkit-ci",
    read_whitelist >>=
    should_be_tested ~target >>= function
    | false -> Term.return "Not authorized"
    | true  -> build ~target >>= test_images
  ]

  (* Test that LinuxKitCI itself builds. *)
  let ci_status target = [
    "build", Git.fetch_head ci_repo target >|= Docker.build ci_dockerfile >>= check_builds;
  ]

  (* For the "linuxkit/linuxkit" GitHub repository, use [status]. *)
  let tests = [
    Config.project ~id:"linuxkit/linuxkit" status;
    Config.project ~id:"linuxkit/linuxkit-ci" ci_status ~dashboards:[];
  ]
end

(* Who is allowed to trigger a rebuild. *)
let can_build =
  let open ACL in
  match profile with
  | `Localhost -> everyone
  | `Production ->
    any [
      username "admin";
      username "github:rneugeba";
      github_org "linuxkit";
    ]

(* Where the archived results URLs in the web UI should point. *)
let state_repo =
  match profile with
  | `Production -> Some (Uri.of_string "https://github.com/linuxkit/linuxkit-logs")
  | `Localhost -> None  (* Don't show links to results on GitHub (since we don't push there) *)

(* Our web UI. *)
let listen_addr =
  (* For production, we're behind an nginx proxy that does https for us.
     For local testing, don't bother with https. *)
  `HTTP 8080

let web_config =
  Web.config
    ~name:"linuxkit-ci"
    ~listen_addr
    ?state_repo
    ~github_scopes_needed:[`Read_org]
    ~can_read:ACL.everyone
    ~can_build
    ()

(* Command-line parsing *)

let () =
  run (Cmdliner.Term.pure (Config.v ~web_config ~projects:Builder.tests))
