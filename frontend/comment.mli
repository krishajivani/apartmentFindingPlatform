type t

val comments_from_json : Yojson.Basic.t -> t

val username : t -> string -> int -> string

val info : t -> string -> int -> string

val string_comments_post : t -> string -> string

