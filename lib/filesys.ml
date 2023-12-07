open Portfolio
module TestPortfolio = Portfolio.UserPortfolio

(** The signature of the user's portfolio data file management sytem. *)
module type FileSysType = sig
  val update_file : string -> Portfolio.UserPortfolio.t -> string
  val to_user_portfolio : string -> Portfolio.UserPortfolio.t
end

module FileSys : FileSysType = struct
  let rec update_file (file : string) (portfolio : Portfolio.UserPortfolio.t) :
      string =
    (* Referred to: https://ocaml.org/docs/file-manipulation *)

    (* Get the output channel. *)
    let oc = open_out file in
    let all_info_list = TestPortfolio.display_portfolio_filesys portfolio in
    let non_title_rows = List.tl all_info_list in
    write_rows oc file non_title_rows

  (** [write_rows oc file rows] uses [oc], which is positioned at the end of
      [file], to write each row in [rows] on a new line in [file]. When function
      has terminated, closes [oc] and returns [file]. Requires: [rows] contains
      valid non-title rows produced from [Portfolio.display_portfolio_filesys]. *)
  and write_rows (oc : out_channel) (file : string) (rows : string list list) :
      string =
    match rows with
    | [] ->
        (* Write nothing now, and no more writing to be done, so close the
           output channel. *)
        close_out oc;
        file
    | first_row :: remaining_rows ->
        let first_data_row_string = string_of_row first_row in
        (* Write the first row. *)
        Printf.fprintf oc "%s\n" first_data_row_string;
        (* Handle remaining rows. *)
        write_rows oc file remaining_rows

  (** [string_of_row row] is a string representation of [row], with each element
      of [row] being ;-separated. Requires: [row] is a valid, non-empty,
      non-title row produced from [Portfolio.display_portfolio_filesys].

      E.g string_of_row
      [["AAPL"; "66"; "32423.2"; "{pb1, qb1, db1}"; "{ps1, qs1, ds1}"]] is
      ["AAPL; 66; 32423.2; {pb1, qb1, db1}; {ps1, qs1, ds1}"]*)
  and string_of_row (row : string list) : string =
    match row with
    | [ ticker; quantity; ibd; bbs; sbs ] ->
        ticker ^ "; " ^ quantity ^ "; " ^ ibd ^ "; " ^ bbs ^ "; " ^ sbs
    | _ -> failwith "Precondition violation."

  let to_user_portfolio (file : string) : Portfolio.UserPortfolio.t =
    (* Referring to: https://ocaml.org/docs/file-manipulation *)

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
end
