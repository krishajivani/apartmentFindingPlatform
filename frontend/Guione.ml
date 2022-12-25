open Search
open Comment
open Post
open Store
open Yojson
open Account

type t = Search.t

let type_pushed = ref "Student"

let self_username = ref "Error"

let self_password = ref "Error"

let get_ref_inc = Random.int (int_of_float (99999999. *. Sys.time ()))

let search_results_specific p =
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

let search_results () =
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
    c_all_data_list id_list

(*Initializes GLib*)
let locale = GMain.init ()

(*y[home_posts] are all posts that are shown in the home screen*)
let home_posts = search_results

(*[window] is GWindow where the main features of the application are. It
  has a home screen, a search screen, and a profile screen.*)
let window = GWindow.window ()

let _ =
  window#resize ~width:1000 ~height:800;
  window#set_title "Cornell Housing Finder"

(*[table_of_tables] is a table with 2 rows and 1 columns inside the main
  window. Multiple other tables are put inside it.*)
let table_of_tables =
  GPack.table ~rows:2 ~columns:1 ~packing:window#add ()

let data_table =
  GPack.table ~rows:1 ~columns:3
    ~packing:(table_of_tables#attach ~left:0 ~top:1 ~expand:`X)
    ()

(*[menu_table] is a table with 1 row and 4 columns packed inside the
  main window. It stores 4 buttons: profile, quit, search, and home.*)
let menu_table =
  GPack.table ~rows:1 ~columns:4
    ~packing:(table_of_tables#attach ~left:0 ~top:0 ~expand:`X)
    ()

let home_table =
  GPack.table ~rows:1 ~columns:1
    ~packing:(data_table#attach ~left:0 ~top:0 ~expand:`X)
    ()

let search_table =
  GPack.table ~rows:2 ~columns:1
    ~packing:(data_table#attach ~left:1 ~top:0 ~expand:`X)
    ()

let search_bar_table =
  GPack.table ~rows:7 ~columns:2
    ~packing:(search_table#attach ~left:0 ~top:0 ~expand:`X)
    ()

let search_label =
  GMisc.label ~text:"Search here"
    ~packing:(search_bar_table#attach ~left:0 ~top:0)
    ()

let search_location_label =
  GMisc.label ~text:"Location:"
    ~packing:(search_bar_table#attach ~left:0 ~top:1)
    ()

let search_location_input =
  GEdit.entry ~packing:(search_bar_table#attach ~left:1 ~top:1) ()

let search_bedrooms_label =
  GMisc.label ~text:"Bedrooms:"
    ~packing:(search_bar_table#attach ~left:0 ~top:2)
    ()

let search_bedroom_input =
  GEdit.entry ~packing:(search_bar_table#attach ~left:1 ~top:2) ()

let search_pricel_label =
  GMisc.label ~text:"Lower price limit:"
    ~packing:(search_bar_table#attach ~left:0 ~top:3)
    ()

let search_pricel_input =
  GEdit.entry ~packing:(search_bar_table#attach ~left:1 ~top:3) ()

let search_priceh_label =
  GMisc.label ~text:"Upper price limit:"
    ~packing:(search_bar_table#attach ~left:0 ~top:4)
    ()

let search_priceh_input =
  GEdit.entry ~packing:(search_bar_table#attach ~left:1 ~top:4) ()

let result_label =
  GMisc.label ~text:"Store result here"
    ~packing:(search_table#attach ~left:0 ~top:1)
    ()

let check_search_input b l pl ph = 7

let make_t () : t =
  let bedrooms = int_of_string search_bedroom_input#text in
  let locations = search_location_input#text in
  let price_l = float_of_string search_pricel_input#text in
  let price_h = float_of_string search_priceh_input#text in

  input_to_query locations bedrooms price_l price_h

let search_button =
  GButton.button ~label:"Search"
    ~packing:(search_bar_table#attach ~left:0 ~top:5)
    ()

let _ =
  search_button#connect#clicked ~callback:(fun () ->
      result_label#set_text
        (let results = search_results_specific (make_t ()) in
         if results = "" then
           if result_label#text = "None found" then "None found again"
           else "None found"
         else results))

let search_dash_label =
  GMisc.label ~text:"--------------------------"
    ~packing:(search_bar_table#attach ~left:0 ~top:6)
    ()

let search_dash_label2 =
  GMisc.label ~text:"------------------------------------------------"
    ~packing:(search_bar_table#attach ~left:1 ~top:6)
    ()

let profile_table =
  GPack.table ~rows:4 ~columns:1
    ~packing:(data_table#attach ~left:2 ~top:0 ~expand:`X)
    ()

let new_post_table =
  GPack.table ~rows:5 ~columns:2
    ~packing:(profile_table#attach ~left:0 ~top:2 ~expand:`X)
    ()

let add_like_table =
  GPack.table ~rows:3 ~columns:2
    ~packing:(profile_table#attach ~left:0 ~top:3 ~expand:`X)
    ()

let add_like_label =
  GMisc.label ~text:"Add a like here"
    ~packing:(add_like_table#attach ~left:0 ~top:0)
    ()

let like_post_label =
  GMisc.label ~text:"Post id:"
    ~packing:(add_like_table#attach ~left:0 ~top:1)
    ()

let like_postid_input =
  GEdit.entry ~packing:(add_like_table#attach ~left:1 ~top:1) ()

let add_like_button =
  GButton.button ~label:"Add like"
    ~packing:(add_like_table#attach ~left:0 ~top:2)
    ()

let _ =
  add_like_button#connect#clicked ~callback:(fun () ->
      add_like like_postid_input#text;
      like_postid_input#set_text "")

let make_post_label =
  GMisc.label ~text:"Make a new post here"
    ~packing:(new_post_table#attach ~left:0 ~top:0)
    ()

let make_post () =
  store_post
    (string_of_int get_ref_inc)
    "ans248" "00011" "West" 3 1000. "Awesome joint"

let post_button =
  GButton.button ~label:"Post"
    ~packing:(new_post_table#attach ~left:0 ~top:2)
    ()

let _ = post_button#connect#clicked ~callback:(fun () -> make_post ())

let post_dash =
  GMisc.label
    ~text:("--------------" ^ "--------------------------")
    ~packing:(new_post_table#attach ~left:1 ~top:3)
    ()

let content_label =
  GMisc.label ~text:"Post content:"
    ~packing:(new_post_table#attach ~left:0 ~top:1)
    ()

let text_input =
  GEdit.entry ~packing:(new_post_table#attach ~left:1 ~top:1) ()

(*[quit_button] is a button inside the menu_table. When clicked it quits
  the GUI.*)
let quit_button =
  GButton.button ~label:"X"
    ~packing:(menu_table#attach ~left:3 ~top:0)
    ()

let _ = quit_button#connect#clicked ~callback:(fun () -> GMain.quit ())

(*[name_label] is a label which displays information.*)
let name_label =
  GMisc.label
    ~text:("Username: " ^ !self_username ^ " \n Type: Owl")
    ~packing:(profile_table#attach ~left:0 ~top:0)
    ()

let profile_dash =
  GMisc.label
    ~text:
      ("----------------------------------------------"
     ^ "--------------------------")
    ~packing:(profile_table#attach ~left:0 ~top:1)
    ()

let feed_label =
  GMisc.label ~text:(home_posts ())
    ~packing:(home_table#attach ~left:0 ~top:0)
    ()

(*[home_button] is a button inside the menu_table. When clicked it
  changes the display to home (where posts are).*)
let menu_home_button =
  GButton.button ~label:"HOME"
    ~packing:(menu_table#attach ~left:0 ~top:0 ~expand:`X)
    ()

let _ =
  menu_home_button#connect#clicked ~callback:(fun () ->
      feed_label#set_text (home_posts ()))

(*[search_button] is a button inside the menu_table. When clicked it
  changes the display to search where users can select filters to look
  for houses.*)
let menu_search_button =
  GButton.button ~label:"SEARCH"
    ~packing:(menu_table#attach ~left:1 ~top:0 ~expand:`X)
    ()

let _ =
  menu_search_button#connect#clicked ~callback:(fun () ->
      print_endline !self_username)

(*[profile_button] is a GButton. When pressed, it displays information
  about the user. *)
let menu_profile_button =
  GButton.button ~label:"PROFILE"
    ~packing:(menu_table#attach ~left:2 ~top:0 ~expand:`X)
    ()

let _ =
  menu_profile_button#connect#clicked ~callback:(fun () ->
      print_endline "profile")

(*[login_window] is a GWindow and the only thing showing when the
  application is launched. It has a login feature and a button to create
  a new account.*)
let login_window = GWindow.window ()

let _ =
  login_window#resize ~width:600 ~height:800;
  login_window#show ();
  login_window#set_title "Cornell Housing Login"

let a = login_window#destroy

(*[signup_window] is a GWindow. It allows a user to make a new
  account.*)
let signup_window = GWindow.window ()

let _ =
  signup_window#resize ~width:400 ~height:400;
  signup_window#set_title "Cornell Housing Sign up"

(*[main_table] is a table with 2 rows and 1 column packed inside the
  login_window. It stores the login_table and button_table inside it.*)
let main_table =
  GPack.table ~rows:2 ~columns:1 ~packing:login_window#add ()

(*[login_table] is a table with 4 rows and 3 columns packed inside the
  first row of the main_table inside the login_window. All labels and
  inputs for login are put inside it.*)
let login_table =
  GPack.table ~rows:4 ~columns:3
    ~packing:(main_table#attach ~left:0 ~top:0)
    ()

(*[warning_labels] is a label which warns the user when they enter an
  invalid username or password. It starts blank until this happens. *)
let warning_label =
  GMisc.label ~text:"" ~packing:(login_table#attach ~left:2 ~top:3) ()

(*[user_label] is a label which reads "username" in the login window*)
let user_label =
  GMisc.label ~text:"Username: "
    ~packing:(login_table#attach ~left:0 ~top:0)
    ()

(*[user_input] is a GEdit which allows the users to enter their username
  while logging in*)
let user_input =
  GEdit.entry ~packing:(login_table#attach ~left:1 ~top:0) ()

(*[pass_label] is a label which reads "password" in the login window*)
let pass_label =
  GMisc.label ~text:"Password: "
    ~packing:(login_table#attach ~left:0 ~top:1)
    ()

(*[pass_input] is a GEdit which allows the users to enter their password
  while logging in*)
let pass_input =
  GEdit.entry ~packing:(login_table#attach ~left:1 ~top:1) ()

let change_credentials a b =
  self_username := a;
  self_password := b

(*[login_callback] is the functionality for when the login button is
  pushed *)
let login_callback () =
  if user_input#text = "" || pass_input#text = "" then
    warning_label#set_text "Username and password may not be empty"
  else if account_valid user_input#text pass_input#text then (
    change_credentials user_input#text pass_input#text;
    name_label#set_text
      ("Username: " ^ !self_username ^ " \n Type:"
      ^ userpass_to_id !self_username !self_password);
    window#show ();
    login_window#destroy ())
  else warning_label#set_text "Invalid username or password"

(*[button_table] is a table with 2 rows and 1 column packed inside the
  main_table inside the login_window. It stores the signupwindow_button
  and the login_button inside it.*)
let button_table =
  GPack.table ~rows:2 ~columns:1
    ~packing:(main_table#attach ~left:0 ~top:1)
    ()

(*[login_button] is a button in the login_window that calls the
  login_callback when clicked.*)
let login_button =
  GButton.button ~label:"Login"
    ~packing:(button_table#attach ~left:1 ~top:0)
    ()

let _ = login_button#connect#clicked ~callback:login_callback

(*[signupwindow_callback] opens up the signup window. *)
let signupwindow_callback () =
  signup_window#show ();
  login_window#destroy ()

(*[signupwindow_button] is a button in the login_window that calls the
  signupwindow_callback when clicked.*)
let signupwindow_button =
  GButton.button ~label:"Sign up"
    ~packing:(button_table#attach ~left:0 ~top:0)
    ()

let _ =
  signupwindow_button#connect#clicked ~callback:signupwindow_callback

(*[login_table] is a table with 3 rows and 3 columns inside the
  signup_window. All labels, inputs, and buttons for signing up are put
  inside it.*)
let signup_table =
  GPack.table ~rows:3 ~columns:3 ~packing:signup_window#add ()

(*[userlandlord_button] is a radio button attached to the signup_table
  in the signup_window. When selected the usertype of the account will
  be landlord.*)
let userlandlord_button =
  GButton.radio_button ~label:"Landlord"
    ~packing:(signup_table#attach ~left:0 ~top:0)
    ()

let _ =
  userlandlord_button#connect#pressed ~callback:(fun () ->
      type_pushed := "Landlord")

(*[userstudent_button] is a radio button attached to the signup_table in
  the signup_window. It's grouped with the other user type so that only
  one can be selected. When selected the usertype of the account will be
  student.*)
let userstudent_button =
  GButton.radio_button ~group:userlandlord_button#group ~label:"Student"
    ~active:true
    ~packing:(signup_table#attach ~left:1 ~top:0)
    ()

let _ =
  userstudent_button#connect#pressed ~callback:(fun () ->
      type_pushed := "Student")

(*[newusername_label] is a label that reads Username inside the
  signup_table in the signup_window.*)
let newusername_label =
  GMisc.label ~text:"Username: "
    ~packing:(signup_table#attach ~left:0 ~top:1)
    ()

(*[newuser_input] is a GEdit entry which allows the users to enter their
  new username while signing up.*)
let newuser_input =
  GEdit.entry ~packing:(signup_table#attach ~left:1 ~top:1) ()

(*[newpass_label] is a label that reads Password inside the signup_table
  in the signup_window.*)
let newpass_label =
  GMisc.label ~text:"Password: "
    ~packing:(signup_table#attach ~left:0 ~top:2)
    ()

(*[newpass_input] is a GEdit entry which allows the users to enter their
  new password while signing up.*)
let newpass_input =
  GEdit.entry ~packing:(signup_table#attach ~left:1 ~top:2) ()

(*[signupwarning_label] is a label that shows up when the user inputs a
  new username that already exists and clicks the signup_button. It's
  inside the signup_table.*)
let signupwarning_label =
  GMisc.label ~text:"" ~packing:(signup_table#attach ~left:2 ~top:2) ()

let signup_callback () =
  if newuser_input#text = "" || newpass_input#text = "" then
    signupwarning_label#set_text
      "Username and password may not be empty"
  else if not (account_valid newuser_input#text newpass_input#text) then (
    store_account
      (string_of_int get_ref_inc)
      newuser_input#text !type_pushed newpass_input#text;
    window#show ();
    signup_window#destroy ())
  else signupwarning_label#set_text "Username already exists"

(*[signup_button] is a button inside the signup_table that takes the
  user to the main page if their inputs are valid or shows the
  signupwarning_label to indicate that the user needs to change
  something.*)
let signup_button =
  GButton.button ~label:"Sign up"
    ~packing:(signup_table#attach ~left:2 ~top:3)
    ()

let _ = signup_button#connect#clicked ~callback:signup_callback

(*Starts the main GUI loop*)
let main () = GMain.Main.main ()

let _ = main ()