val store_account : string -> string -> string -> string -> unit
(** store_account user_id username user_type password stores an account.*)

val account_valid : string -> string -> bool
(** account_valid username password checks to see if there an account
    stored with the specified username and password. If there is,
    account_valid returns true, and if it is not, it returns false. *)

val user_type : string -> string
(** user_type user_id takes in a user id and returns the user type. *)

val userpass_to_id : string -> string -> string
(** account_valid username password takes in a username and password and
    returns the user type. *)
