open Portfolio
module TestPortfolio = Portfolio.UserPortfolio

(** The signature of the user's portfolio data file management sytem. *)
module type FileSysType = sig
  val create_file : string
  val update_file : string -> string
  val to_user_portfolio : string -> 'a Portfolio.UserPortfolio.t
  val to_file : 'a Portfolio.UserPortfolio.t -> string
end

module FileSys : FileSysType = struct
  (** The data file created has name [data.txt], is stored in a new directory
      [data_dir]. *)
  let create_file = failwith "unimplemented"

  let update_file (file : string) : string = failwith "unimplemented"

  let to_user_portfolio (file : string) : 'a Portfolio.UserPortfolio.t =
    (* Referring to: https://ocaml.org/docs/file-manipulation*)

    (* Currently trying to just read from a [.txt] file that contains a single
       line, with the name of the stock ticker. *)
    let portfolio = TestPortfolio.empty_portfolio in
    (* Read the first line. *)
    let ic = open_in file in

    try
      let stock = input_line ic in
      close_in ic;
      TestPortfolio.add_stock portfolio stock
        100 (* Just add some random quantity. *)
    with e ->
      (* some unexpected error occurs *)
      close_in_noerr ic;
      (* emergency closing *)
      raise e

  let to_file (portfolio : 'a Portfolio.UserPortfolio.t) : string =
    failwith "unimplemented"
end
