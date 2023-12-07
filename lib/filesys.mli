open Portfolio

(** The user's portfolio data is stored in
    [CS-3110-Final-Project---zaz/data_dir/data.txt]. Ensure this
    data_dir/data.txt location exists before running the file system. *)

(** The signature for the user's portfolio data file management system. *)
module type FileSysType = sig
  val update_file : string -> Portfolio.UserPortfolio.t -> string
  (** [update_file file portfolio] updates (overwrites) [file] to store exactly
      all the data in [portfolio]. The file name is returned.*)

  val to_user_portfolio : string -> Portfolio.UserPortfolio.t
  (** [to_user_portfolio file] uses exactly all the data in [file] to create a
      [UserPortfolio]. *)
end

module FileSys : FileSysType
(** Enables management of user's portfolio data file. *)
