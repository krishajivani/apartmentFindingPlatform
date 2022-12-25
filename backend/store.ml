open Yojson
open Yojson.Basic
open Yojson.Basic.Util
open Post

let post_data = "./post_data.json"

let post_data_json = from_file post_data

let student_comments = "./comments_students_data.json"

let student_comments_json = from_file student_comments

let all_comments = "./comments_all_data.json"

let all_comments_json = from_file all_comments

let likes_json = from_file "./likes.json"

let default json str = json
(* let default json str = if postboard_from_json json = empty then
   `String str else member str json *)

let j = default post_data_json "posts"

let c_students = default student_comments_json "comments"

let c_all = default all_comments_json "comments"

let likes_of_json json =
  List.map (fun x -> json |> member x |> to_int) (keys json)

let likes = likes_of_json likes_json

let likes_json_helper lst =
  List.init (List.length lst) (fun x ->
      `Assoc
        [
          ( string_of_int (List.length lst - 1 - x),
            `Int (List.nth lst x) );
        ])

let likes_to_json lst =
  List.fold_left combine (`Assoc []) (likes_json_helper lst)

let add_like post_id =
  let lst =
    if List.length likes - 1 < int_of_string post_id then 1 :: likes
    else
      List.mapi
        (fun i x ->
          if i = List.length likes - 1 - int_of_string post_id then
            x + 1
          else x)
        likes
  in
  to_file "./likes.json" (likes_to_json lst)

let combine_json t r file =
  let result =
    match r with
    | `String _ -> t
    | _ -> combine r t
  in
  to_file file result

let store_post
    (post_id : string)
    (username : string)
    (user_id : string)
    (location : string)
    (bedrooms : int)
    (price : float)
    (post_info : string) =
  let jsn =
    `Assoc
      [
        ( post_id,
          `Assoc
            [
              ("post_id", `String post_id);
              ( "user",
                `Assoc
                  [
                    ("username", `String username);
                    ("user_id", `String user_id);
                    ("user_type", `String "landlord");
                  ] );
              ("location", `String location);
              ("bedrooms", `Int bedrooms);
              ("price", `Float price);
              ("post_info", `String post_info);
            ] );
      ]
  in

  combine_json jsn j post_data

(*Adding comment for the sake of it.*)

let comment_jsn username post_id position comment_info =
  `Assoc
    [
      ( string_of_int position ^ post_id,
        `Assoc
          [
            ("username", `String username);
            ("post_id", `String post_id);
            ("position", `Int position);
            ("info", `String comment_info);
          ] );
    ]

let store_student_comment username post_id position comment_info =
  let jsn = comment_jsn username post_id position comment_info in
  combine_json jsn c_students student_comments

let store_public_comment username post_id position comment_info =
  let jsn = comment_jsn username post_id position comment_info in
  combine_json jsn c_all all_comments
