open Yojson
open Yojson.Basic
open Yojson.Basic.Util

exception UnknownUserType of string

exception UnknownUserId of string

type t = {
  user_id : string;
  username : string;
  password : string;
  user_type : string;
}

let combine_json t r file =
  let result =
    match r with
    | `String _ -> t
    | _ -> combine r t
  in
  to_file file result

let default json str = json

let accounts = "./accounts.json"

let accounts_json = from_file accounts

let j = default accounts_json "accounts"

let store_account
    (user_id : string)
    (username : string)
    (user_type : string)
    (password : string) =
  let jsn =
    `Assoc
      [
        ( user_id,
          `Assoc
            [
              ("user_id", `String user_id);
              ("username", `String username);
              ("user_type", `String user_type);
              ("password", `String password);
            ] );
      ]
  in

  combine_json jsn j accounts

let account_of_json json =
  {
    user_id = json |> member "user_id" |> to_string;
    username = json |> member "username" |> to_string;
    user_type = json |> member "user_type" |> to_string;
    password = json |> member "password" |> to_string;
  }

let str_is_int s =
  try int_of_string s |> string_of_int = s with
  | Failure _ -> false

let account_keys json = List.filter (fun x -> str_is_int x) (keys json)

let account_list_from_json json =
  List.map
    (fun x -> json |> member x |> account_of_json)
    (account_keys json)

let return_user_id account = account.user_id

let accounts_list = account_list_from_json accounts_json

let user_ids = List.map return_user_id accounts_list

exception UnknownAccount of string

let rec id_to_account lst id =
  match lst with
  | [] -> raise (UnknownAccount id)
  | h :: t -> if h.user_id = id then h else id_to_account t id

let username id = (id_to_account accounts_list id).username

let password id = (id_to_account accounts_list id).password

let user_type id = (id_to_account accounts_list id).user_type

let account_to_query id =
  {
    username = username id;
    password = password id;
    user_id = id;
    user_type = user_type id;
  }

let rec account_valid_tr ids usern passw =
  match ids with
  | [] -> false
  | h :: t ->
      if
        (account_to_query h).username = usern
        && (account_to_query h).password = passw
      then true
      else account_valid_tr t usern passw

let account_valid usern passw = account_valid_tr user_ids usern passw

let rec userpass_to_id_tr ids usern passw =
  match ids with
  | [] -> raise (UnknownAccount (usern ^ " " ^ passw))
  | h :: t ->
      if
        (account_to_query h).username = usern
        && (account_to_query h).password = passw
      then h
      else userpass_to_id_tr t usern passw

let userpass_to_id usern passw = userpass_to_id_tr user_ids usern passw