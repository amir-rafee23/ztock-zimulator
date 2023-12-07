open Portfolio
module TestPortfolio = Portfolio.UserPortfolio

(** The signature of the user's portfolio data file management sytem. *)
module type FileSysType = sig
  val update_file : string -> Portfolio.UserPortfolio.t -> string
  val to_user_portfolio : string -> Portfolio.UserPortfolio.t
end

(* Convert representation type to a string list list. *)

module FileSys : FileSysType = struct
  let update_file (file : string) (portfolio : Portfolio.UserPortfolio.t) :
      string =
    (* Referring to: https://ocaml.org/docs/file-manipulation *)

    (* Get the output channel. *)
    let oc = open_out file in
    let all_info = String_map.bindings portfolio in
    failwith "unimplemented"

  (* Get the first key-value pair. *)
  (* let all_info = String_map.bindings portfolio in failwith "unimplemented" *)

  (* Get the output channel. let oc = open_out file in (* Write something. *)
     Printf.fprintf oc "hi!"; (* Close the output channel. *) close_out oc; (*
     Return the file name. *) file *)

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
