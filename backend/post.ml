open Yojson
open Yojson.Basic.Util

type person_type =
  | Student
  | Landlord
      (** The varient type of a person, a Student or a Landlord. *)

exception UnknownUserType of string

exception UnknownUserId of string

type user = {
  username : string;
  user_id : string;
  user_type : person_type;
}
(** The type of users, a record consisting of fields username, user
    identifier, and user type. *)

include Comment

type comments = Comment.t

(** The type of comments, a record consisting of fields user id, post
    id, position, and comment information. *)

type post = {
  post_id : string;
  user : user;
  location : string;
  bedrooms : int;
  price : float;
  post_info : string; (*likes : int;*)
}
(** The type of posts, a record consisting of fields post identifier,
    user, location, number of bedrooms, renting price, post information,
    number of likes, student comments section, and all user comments
    section. *)

type t = post list

(** helper function to parse the JSON into type user*)
let user_of_json json =
  {
    username = json |> member "username" |> to_string;
    user_id = json |> member "user_id" |> to_string;
    user_type =
      (match json |> member "user_type" |> to_string with
      | "landlord" -> Landlord
      | "student" -> Student
      | x -> raise (UnknownUserType x));
  }

(** helper function to parse the JSON into type comment (commented out
    because we replaced it with one in comment.ml) let comment_of_json
    json = { user_id = json |> member "user_id" |> to_string;
    comment_info = json |> member "comment" |> to_string; }*)

let empty : t = []

(** helper function to parse the JSON into type post*)
let post_of_json json =
  {
    post_id = json |> member "post_id" |> to_string;
    user = json |> member "user" |> user_of_json;
    location = json |> member "location" |> to_string;
    bedrooms = json |> member "bedrooms" |> to_int;
    price = json |> member "price" |> to_float;
    post_info =
      json |> member "post_info" |> to_string
      (*likes = json |> member "likes" |> to_int;*);
  }

(**DELETE THIS: let postboard_from_json json = json |> member "posts" |>
   to_list |> List.map post_of_json.......post_id = json |> member
   "post_id" |> to_string; *)

(*let single_value json = fold_left (json |> member "postA") [] json *)

(*let plz_work json = for x = 0 to !post_counter do json |> member
  (string_of_int !post_counter) done *)

(* let postboard_from_json json = json |> member "posts" |> to_list |>
   List.map post_of_json *)
(* let len json = List.length json *)

let str_is_int s =
  try int_of_string s |> string_of_int = s with
  | Failure _ -> false

let post_keys json = List.filter (fun x -> str_is_int x) (keys json)

let postboard_from_json json =
  List.map (fun x -> json |> member x |> post_of_json) (post_keys json)

(* let plz_work json = for x = 0 to len json do json |> member
   (string_of_int (len json)) |> to_list |> List.map post_of_json
   done *)
(*let postboard_from_json json = json |> to_assoc|> to_list |> List.map
  post_of_json *)
(* let postboard_from_json json = json |> values |> flatten |> List.map
   post_of_json *)

let return_post_id room = room.post_id

let post_ids pstbrd = List.map return_post_id pstbrd

exception UnknownPost of string

let rec id_to_post lst id =
  match lst with
  | [] -> raise (UnknownPost id)
  | h :: t -> if h.post_id = id then h else id_to_post t id

let user_type_to_string user_type =
  match user_type with
  | Student -> "Student"
  | Landlord -> "Landlord"

let user_to_string { username; user_id; user_type } =
  user_type_to_string user_type
  ^ " " ^ username ^ " (" ^ user_id ^ ") \n"

let user pstbrd id = user_to_string (id_to_post pstbrd id).user

let location pstbrd id = (id_to_post pstbrd id).location

let bedrooms pstbrd id = (id_to_post pstbrd id).bedrooms

let price pstbrd id = (id_to_post pstbrd id).price

let post_info pstbrd id = (id_to_post pstbrd id).post_info

(*let likes pstbrd id = (id_to_post pstbrd id).likes*)

(** This is commented out because it is now all dealt with in the
    comment compilation unit. let rec string_of_comments (comment_list :
    comment list) : string = match comment_list with | [] -> "" | {
    user_id; comment_info } :: t -> "User: " ^ user_id ^ "\n Comment: "
    ^ comment_info ^ "\n" ^ string_of_comments t

    let comments_students comments id = Comment.string_comments_post
    comments id

    let comments_all pstbrd id = string_of_comments (id_to_post pstbrd
    id).comments_all*)
