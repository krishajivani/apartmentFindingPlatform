type t = {
  location : string;
  bedrooms : int;
  price : float * float;
}
(** The record type of the criteria of a search query, including
    location, number of bedrooms, and price range. *)

(*INPUT TO QUERY START*)

let input_to_query loc rooms price_min price_max =
  { location = loc; bedrooms = rooms; price = (price_min, price_max) }

(*INPUT TO QUERY END*)

(*POST TO QUERY START*)

open Post
open Comment

exception Invalid_Price_Range

let post_to_query pstbrd id =
  let query_price (min, max) =
    {
      location = location pstbrd id;
      bedrooms = bedrooms pstbrd id;
      price = (min, max);
    }
  in
  match price pstbrd id with
  | x when x >= 0. && x < 500. -> query_price (0., 500.)
  | x when x >= 500. && x < 1000. -> query_price (500., 1000.)
  | x when x >= 1000. && x < 1500. -> query_price (1000., 1500.)
  | x when x >= 1500. && x < 2000. -> query_price (1500., 2000.)
  | x when x >= 2000. && x < 2500. -> query_price (2000., 2500.)
  | x when x >= 2500. && x < 3000. -> query_price (2500., 3000.)
  | _ -> raise Invalid_Price_Range

(*POST TO QUERY END*)

(*SUITABLE POSTS START*)

let rec suitable_posts_tr pstbrd ids query acc =
  match ids with
  | [] -> acc
  | h :: t ->
      if post_to_query pstbrd h = query then
        suitable_posts_tr pstbrd t query (h :: acc)
      else suitable_posts_tr pstbrd t query acc

let suitable_posts pstbrd ids query =
  suitable_posts_tr pstbrd ids query []

(*SUITABLE POSTS END*)

let post_to_string pstbrd c_students c_all id =
  "Post id: " ^ id ^ "\n" ^ user pstbrd id ^ "\n" ^ location pstbrd id
  ^ "\n"
  ^ string_of_int (bedrooms pstbrd id)
  ^ "\n"
  ^ string_of_float (price pstbrd id)
  ^ "\n" ^ post_info pstbrd id ^ "\n"
  ^ string_comments_post c_students id
  ^ "\n"
  ^ string_comments_post c_all id
  ^ "\n" ^ "\n"

let posts_to_string pstbrd c_students c_all (posts : string list) =
  List.fold_left ( ^ ) ""
    (List.map (post_to_string pstbrd c_students c_all) posts)
