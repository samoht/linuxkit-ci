(** Manage pull-request state. *)

open Datakit_ci

type t
(** The type for pull-request state manager. *)

val make: logs:Live_log.manager -> t
(** [make ~logs] create a new pull-request manager. *)

(** The type for pull-request state. *)
type v = [
  | `OkToTest
  | `InWhiteList
  | `NotAuthorized
]

val v: t -> Whitelist.v -> Datakit_github.PR.t -> v Term.t
(** [v t ~whitelist pr] is a term which evaluates to state of the
    pull-request [pr] using the given [whitelist]. *)
