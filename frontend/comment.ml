type comment = {
  username : string;
  post_id : string;
  position : int;
  info : string;
}

type t = comment list

open Yojson
open Yojson.Basic.Util

let comment_of_json json =
  {
    username = json |> member "username" |> to_string;
    post_id = json |> member "post_id" |> to_string;
    position = json |> member "position" |> to_int;
    info = json |> member "info" |> to_string;
  }

let str_is_int s =
  try int_of_string s |> string_of_int = s with
  | Failure _ -> false

let comment_keys json = List.filter (fun x -> str_is_int x) (keys json)

(*let comments_from_json json = json |> member "comments" |> to_list |>
  List.map comment_of_json *)
let comments_from_json json =
  List.map
    (fun x -> json |> member x |> comment_of_json)
    (comment_keys json)

exception UnknownComment of string

let rec return_comment lst id position =
  match lst with
  | [] -> raise (UnknownComment (id ^ string_of_int position))
  | h :: t ->
      if h.post_id = id && h.position = position then h
      else return_comment t id position

let username comments post_id position =
  (return_comment comments post_id position).username

let info comments post_id position =
  (return_comment comments post_id position).info

let rec comments_of_post_tr lst id acc =
  match lst with
  | [] -> acc
  | h :: t ->
      if h.post_id = id then h :: acc else comments_of_post_tr t id acc

let comments_of_post lst id = comments_of_post_tr lst id []

let rec string_of_comments (comment_list : comment list) : string =
  match comment_list with
  | [] -> ""
  | { username; info } :: t ->
      "User: " ^ username ^ "\n Comment: " ^ info ^ "\n"
      ^ string_of_comments t

let string_comments_post lst id =
  comments_of_post lst id |> string_of_comments
