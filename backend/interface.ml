open Post
open Comment
open Search
open Yojson
open Store
open Account

(** TO SEARCH FOR POSTS, UNCOMMENT THE CODE BELOW *)

let search_results p =
  let post_data = Yojson.Basic.from_file "./post_data.json" in
  let post_data_postboard = postboard_from_json post_data in
  let c_students_data =
    Yojson.Basic.from_file "./comments_students_data.json"
  in
  let c_students_data_list = comments_from_json c_students_data in
  let c_all_data = Yojson.Basic.from_file "./comments_all_data.json" in
  let c_all_data_list = comments_from_json c_all_data in
  let id_list = post_ids post_data_postboard in
  posts_to_string post_data_postboard c_students_data_list
    c_all_data_list
    (suitable_posts post_data_postboard id_list p)

(** [interface ()] prompts for the location, bedrooms, and price range,
    then outputs matched posts based on the inputs. *)

(** --------------------------------------------------------------*)
let interface () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the Apartment Searching Platform.\n";
  print_endline
    "Please enter the search location, number of bedrooms, and price\n\
    \ range in this format: [location] [bedrooms]\n\
    \ [min_bound,max_bound].\n";
  print_string "> ";
  let p =
    Scanf.scanf "%s %i %f,%f" (fun l r p_min p_max ->
        input_to_query l r p_min p_max)
  in
  print_endline (search_results p);
  print_newline ()

(** TO MAKE AND STORE POSTS, UNCOMMENT THE CODE BELOW *)

(**let rec loop () = let r = read_line () |> String.split_on_char ' ' in
   let a = List.nth r 0 in let b = List.nth r 1 in let c = List.nth r 2
   in let d = List.nth r 3 in let e = int_of_string (List.nth r 4) in
   let f = float_of_string (List.nth r 5) in let g = List.nth r 6 in
   store_post a b c d e f g; print_endline "Please enter the\n\ \
   following\n\ \ information in this format: [post_id] [username]\n\ \
   [user_id]\n\ \ [location] [bedrooms] [price] [post_info]";
   print_string "> "; loop ()

   let interface () = ANSITerminal.print_string [ ANSITerminal.red ]
   "\n\nWelcome to Apartment Post Creating and Storing.\n";
   print_endline "Please enter the following information in this\n\ \
   format: [post_id] [username] [user_id] [location] [bedrooms]\n\ \
   [price] [post_info]"; print_string "> "; loop () *)

(** -------------------------------------------------------------- *)

(** TO MAKE AND STORE A STUDENT COMMENT, UNCOMMENT THE CODE BELOW *)

(** let rec loop () = let r = read_line () |> String.split_on_char ' '
    in let a = List.nth r 0 in let b = List.nth r 1 in let c =
    int_of_string (List.nth r 2) in let d = List.nth r 3 in
    store_student_comment a b c d; print_endline "Please enter the\n\ \
    following information in this format: [username] [post_id]\n\ \
    [position] [info]"; print_string "> "; loop ()

    let interface () = ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Student Comment Creating and Storing.\n";
    print_endline "Please enter the following information in this\n\ \
    format: [username] [post_id] [position] [info]"; print_string ">\n\n
    "; loop () *)

(** ------------------------------------------------------------- *)

(** TO MAKE AND STORE A PUBLIC COMMENT, UNCOMMENT THE CODE BELOW *)

(** let rec loop () = let r = read_line () |> String.split_on_char ' '
    in let a = List.nth r 0 in let b = List.nth r 1 in let c =
    int_of_string (List.nth r 2) in let d = List.nth r 3 in
    store_public_comment a b c d; print_endline "Please enter the\n\ \
    following information in this format: [username] [post_id]\n\ \
    [position] [info]"; print_string "> "; loop ()

    let interface () = ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Public Comment Creating and Storing.\n";
    print_endline "Please enter the following information in this\n\ \
    format: [username] [post_id] [position] [info]"; print_string ">\n\n
    "; loop () *)

(** ---------------------------------------------------------------- *)

(** TO ADD AND STORE A LIKE TO A POST, UNCOMMENT THE CODE BELOW *)

(*let rec loop () = let r = read_line () in add_like r; print_endline
  "Please enter the following information in this format: [post_id]";
  print_string "> "; loop ()

  let interface () = ANSITerminal.print_string [ ANSITerminal.red ]
  "\n\nWelcome to Post Like Creating and Storing.\n"; print_endline
  "Please enter the following information in this format: [post_id]";
  print_string "> "; loop () *)

(** ------------------------------------------------------------------ *)

(** TO ADD AND STORE A NEW ACCOUNT, UNCOMMENT THE CODE BELOW*)

(*let rec loop () = let r = read_line () |> String.split_on_char ' ' in
  let a = List.nth r 0 in let b = List.nth r 1 in let c = List.nth r 2
  in let d = List.nth r 3 in store_account a b c d; print_endline
  "Please enter the\n\ \ following\n\ \ information in this format:\n\ \
  [user_id] [username] [user_type] [password]"; print_string "> "; loop
  ()

  let interface () = ANSITerminal.print_string [ ANSITerminal.red ]
  "\n\nWelcome to Apartment Account Creating and Storing.\n";
  print_endline "Please enter the following information in this\n\ \
  format: [user_id] [username] [user_type] [password]"; print_string
  ">\n "; loop () *)

(* ------------------------------------------------------------------ *)

(*TO SEARCH FOR AN ACCOUNT, UNCOMMENT THE CODE BELOW*)

(*let interface () = ANSITerminal.print_string [ ANSITerminal.red ]
  "\n\nWelcome to the Account Searching Platform.\n"; print_endline
  "Please enter the username and password in this format: [username]
  [password].\n\ \ "; print_string ">\n "; let r = read_line () |>
  String.split_on_char ' ' in let a = List.nth r 0 in let b = List.nth r
  1 in print_endline (Bool.to_string (account_valid a b)); print_newline
  () *)

(* Execute user interface mode. *)
let () = interface ()
