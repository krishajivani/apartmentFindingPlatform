type t
(** The abstract type of values representing a postboard. *)

val empty : t

val postboard_from_json : Yojson.Basic.t -> t
(** [postboard_from_json j] is the postboard that [j] represents.
    Requires: [j] is a valid JSON postboard representation. *)

val post_ids : t -> string list
(** [postboard_from_json j] is the postboard that [j] represents.
    Requires: [j] is a valid JSON postboard representation. *)

val user : t -> string -> string

val location : t -> string -> string
(** [postboard_from_json j] is the postboard that [j] represents.
    Requires: [j] is a valid JSON postboard representation. *)

val bedrooms : t -> string -> int
(** [postboard_from_json j] is the postboard that [j] represents.
    Requires: [j] is a valid JSON postboard representation. *)

val price : t -> string -> float
(** [postboard_from_json j] is the postboard that [j] represents.
    Requires: [j] is a valid JSON postboard representation. *)

val post_info : t -> string -> string
(** [postboard_from_json j] is the postboard that [j] represents.
    Requires: [j] is a valid JSON postboard representation. *)

(*val likes : t -> string -> int*)
(** [postboard_from_json j] is the postboard that [j] represents.
    Requires: [j] is a valid JSON postboard representation. *)

(**val comments_students : t -> string -> string

   val comments_all : t -> string -> string*)
