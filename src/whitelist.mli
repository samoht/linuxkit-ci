(** Manage whitelists *)

open Datakit_ci

type t
(** The type for whitelist managers. *)

val make: logs:Live_log.manager -> t
(** [make ~logs] create a new whitelist manager. *)

type v
(** The type for whitelists. *)

val v: t -> Git.commit -> v Term.t
(** [v t c] is the whitelist stored at the root of the commit [c]. *)

val mem: string -> v -> bool
(** [mem name w] checks whether [name] belongs to the whitelist
    [w]. *)

val hash: v -> string
(** [hash w] is a deterministic hash of the whitelist [w]. *)
