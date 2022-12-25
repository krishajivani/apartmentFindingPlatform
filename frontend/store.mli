open Yojson

val store_post :
  string -> string -> string -> string -> int -> float -> string -> unit
(** [json_of_post post_id username user_id location bedrooms price post_info]
    stores a new post as a Yojson.Basic.t type in the post_data json
    file. *)

val store_student_comment : string -> string -> int -> string -> unit

val store_public_comment : string -> string -> int -> string -> unit

val add_like : string -> unit