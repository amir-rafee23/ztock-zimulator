open Portfolio

(** The signature for the user's portfolio data file management system. *)
module type FileSysType = sig
  val create_file : string
  (** Creates a new [.txt] file named [data.txt] to store the user's portfolio
      data. The file name is returned.*)

  val update_file : string -> string
  (** [update_file file] updates [file] to contain most recent data on the
      user's portfolio. The file name is returned.*)

  val to_user_portfolio : string -> 'a Portfolio.UserPortfolio.t
  (** [to_user_portfolio file] uses exactly all the data in [file] to create a
      [UserPortfolio]. *)

  val to_file : 'a Portfolio.UserPortfolio.t -> string
  (** [to_file portfolio] creates a data file named [data.txt] that stores
      exactly all the information in [portfolio]. The file name is returned. *)
end

module FileSys : FileSysType
(** Enables management of user's portfolio data file. *)
