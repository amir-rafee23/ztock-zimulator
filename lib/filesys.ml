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

  let rec to_user_portfolio (file : string) : Portfolio.UserPortfolio.t =
    (* Referred to: https://ocaml.org/docs/file-manipulation *)

    (* Get the input channel, positioned at the start of [file]. *)
    let ic = open_in file in
    (* Portfolio to be returned. *)
    let portfolio = TestPortfolio.empty_portfolio in
    (* Since [ic] is positioned at the start of [file], [portfolio] is updated
       to contain exactly all the data in [file], from start to end.*)
    read_rows ic file portfolio

  (** [read_rows ic file portfolio] updates portfolio to contain exactly all the
      data in [file] from the current position of [ic] to the end of [file]. *)
  and read_rows (ic : in_channel) (file : string)
      (portfolio : Portfolio.UserPortfolio.t) : Portfolio.UserPortfolio.t =
    try
      (* Read the first line, discard \n. *)
      let line = input_line ic in
      (* Add the first line's data to [portfolio]. *)
      let portfolio' = add_line_to_portfolio line portfolio in
      (* Add the remaining lines. *)
      let portfolio'' = read_rows ic file portfolio' in
      portfolio''
    with e ->
      (* Reached the end of [file], meaning [file] was empty, so no updating of
         [portfolio]. *)
      if e = End_of_file then (
        close_in ic;
        portfolio)
      else (
        (* Some unexpected exception occurs. *)
        close_in_noerr ic;
        (* Emergency closing. *)
        raise e)

  (** [add_line_to_portfolio line portfolio] updates [portfolio] to contain
      exactly all the information in [line], and returns it. Requires: [line] is
      a valid line from a [.txt] file created using [update_file]. *)
  and add_line_to_portfolio (line : string)
      (portfolio : Portfolio.UserPortfolio.t) : Portfolio.UserPortfolio.t =
    match line with
    | "" -> portfolio
    | line ->
        (* Extract data from [line]. *)
        let all_line_data =
          String.split_on_char ';' line |> List.map (fun s -> String.trim s)
        in
        (* Key to be inserted into [portfolio]. *)
        let key : string = List.nth all_line_data 0 in
        (* Build up the corresponding value to be inserted into [portfolio]. *)
        let q = List.nth all_line_data 1 |> int_of_string in
        let ibd = List.nth all_line_data 2 |> float_of_string in
        let bbs = List.nth all_line_data 3 |> TestPortfolio.batches_of_string in
        let sbs = List.nth all_line_data 4 |> TestPortfolio.batches_of_string in
        (* Corresponding value to be inserted into [portfolio]. *)
        let value : Portfolio.UserPortfolio.stock_data =
          {
            quantity = q;
            initial_buy_date = ibd;
            buy_batches = bbs;
            sell_batches = sbs;
          }
        in
        (* Insert the key-value pair. *)
        let portfolio' = String_map.add key value portfolio in
        (* Final result. *)
        portfolio'
end
