type t
(** The abstract type of values representing a search query. *)

val input_to_query : string -> int -> float -> float -> t
(** [input_to_query j] is the query representing the user's input.
    Requires: [j] is a valid JSON postboard representation. *)

val post_to_query : Post.t -> string -> t
(** [post_to_query j] is the query data interpreted from a user's post. *)

val suitable_posts : Post.t -> string list -> t -> string list
(** [suitable_posts] is the list of posts matching the user's desired
    input. *)

val posts_to_string :
  Post.t -> Comment.t -> Comment.t -> string list -> string

val posts_to_string_list :
  Post.t -> Comment.t -> Comment.t -> string list -> string list
