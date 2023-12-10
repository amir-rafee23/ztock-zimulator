(* Test Plan.*)

(* 1. Parts of system automatically tested by OUnit. *)

(*- [portfolio.ml]: All the functions in the [.mli], except for those that
  return output containing information on dates/prices. These excluded functions
  are [display_portfolio], [display_portfolio_filesys]. The reason for these
  exclusions is the dates/prices depend on the current time the user polls the
  API. so these data are not known ahead of time. Black-box testing utilized
  throughout. *)

(* [filesys.ml]: For all the functions in the [.mli], a simple test case is
   provided. More complicated test cases were attempted, but they were taking
   too long to run, possibly because checking that the underlying portfolios are
   equal is computationally expensive. However, throughout interactive testing,
   the file-saving system did not hang unexpectedly (after additional fixes were
   made on the UI end). This simple test case utilized black-box testing, the
   interactive tests utilized glass-box testing. *)

(* 2. Parts of the system manually tested. *)

(* - [portfolio.ml]: [display_portfolio], [display_portfolio_filesys]. These
   were tested extensively interactively via utop, both with and without the
   UI. *)
(* - [filesys.ml]: All the functions in the [.mli] file, and therefore their
   helpers, were tested extensively interactively via utop, both with and
   without the UI. *)

(*3. Why testing demonstrates the correctness of the system. *)

(* - Testing was done continuously throughout development, including interactive
   testing. *)
(* - The backend was tested independently of the front end, as well as
   together. *)
(* Independent testing was done on each team member's machine, to root out bugs
   that may have arisen due to personal configurations. *)

open OUnit2
open Stocks
open Portfolio
open Unix
module Test_Portfolio = Portfolio.UserPortfolio
open Filesys
module Test_Filesys = FileSys

(* The following two functions were taken from A2. *)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [pp_portfolio] pretty-prints [portfolio]. *)
let pp_portfolio portfolio =
  Test_Portfolio.display_portfolio portfolio |> pp_list pp_string

let contains_stock_tests =
  [
    ( "Empty portfolio" >:: fun _ ->
      assert_equal false
        (Test_Portfolio.contains_stock Test_Portfolio.empty_portfolio "MSFT") );
  ]

(* Portfolios used to test [add_stock]. *)
let add_stock_test_portfolio_1 =
  Test_Portfolio.add_stock Test_Portfolio.empty_portfolio "MSFT" 10

let add_stock_test_portfolio_2 =
  Test_Portfolio.add_stock add_stock_test_portfolio_1 "AAPL" 5

let add_stock_test_portfolio_3 =
  Test_Portfolio.add_stock add_stock_test_portfolio_2 "MSFT" 6

let add_stock_test_portfolio_4 =
  Test_Portfolio.add_stock
    (Test_Portfolio.add_stock
       (Test_Portfolio.add_stock add_stock_test_portfolio_3 "GOOG" 12)
       "AAPL" 3)
    "MSFT" 1

(* TODO: More comprehensive tests. *)
(* Also tests [quantity_stock] and [contains_stock]. *)
let add_stock_tests =
  [
    ( " Adding stock s to empty portfolio - presence of s. " >:: fun _ ->
      assert_equal true
        (Test_Portfolio.contains_stock add_stock_test_portfolio_1 "MSFT") );
    ( " Adding stock s to empty portfolio - quantity of s. " >:: fun _ ->
      assert_equal 10
        (Test_Portfolio.quantity_stock add_stock_test_portfolio_1 "MSFT") );
    ( " Adding stock to empty portfolio - presence of absent stock. "
    >:: fun _ ->
      assert_equal false
        (Test_Portfolio.contains_stock add_stock_test_portfolio_1 "XYZ") );
    ( " Adding new stock s to non-empty portfolio - presence of s. " >:: fun _ ->
      assert_equal true
        (Test_Portfolio.contains_stock add_stock_test_portfolio_2 "AAPL") );
    ( " Adding new stock s to non-empty portfolio - quantity of s. " >:: fun _ ->
      assert_equal 5
        (Test_Portfolio.quantity_stock add_stock_test_portfolio_2 "AAPL") );
    ( " Adding new stock to non-empty portfolio - quantity of absent stock. "
    >:: fun _ ->
      assert_equal 0
        (Test_Portfolio.quantity_stock add_stock_test_portfolio_2 "XYZ") );
    ( " Adding existing stock s to non-empty portfolio - quantity of s. "
    >:: fun _ ->
      assert_equal 16
        (Test_Portfolio.quantity_stock add_stock_test_portfolio_3 "MSFT") );
    ( " Adding existing stock to non-empty portfolio - quantity of unadded \
       stock unchanged. "
    >:: fun _ ->
      assert_equal 5
        (Test_Portfolio.quantity_stock add_stock_test_portfolio_3 "AAPL") );
    ( " More complex sequence of adding - 1" >:: fun _ ->
      assert_equal 12
        (Test_Portfolio.quantity_stock add_stock_test_portfolio_4 "GOOG") );
    ( " More complex sequence of adding - 2" >:: fun _ ->
      assert_equal 8
        (Test_Portfolio.quantity_stock add_stock_test_portfolio_4 "AAPL") );
    ( " More complex sequence of adding - 3" >:: fun _ ->
      assert_equal 17
        (Test_Portfolio.quantity_stock add_stock_test_portfolio_4 "MSFT") );
  ]

(* Portfolios used to test [remove_stock]. *)
let remove_stock_test_portfolio1 =
  let p1 = Test_Portfolio.add_stock Test_Portfolio.empty_portfolio "AAPL" 10 in
  p1

let remove_stock_test_portfolio2 =
  let p1 = Test_Portfolio.add_stock Test_Portfolio.empty_portfolio "MSFT" 5 in
  let p2 = Test_Portfolio.remove_stock p1 "MSFT" 2 in
  p2

let remove_stock_test_portfolio3 =
  Test_Portfolio.remove_stock remove_stock_test_portfolio2 "MSFT" 3

let remove_stock_test_portfolio4 =
  Test_Portfolio.remove_stock
    (Test_Portfolio.add_stock Test_Portfolio.empty_portfolio "AAPL" 10)
    "AAPL" 7

let remove_stock_test_portfolio5 =
  Test_Portfolio.remove_stock
    (Test_Portfolio.remove_stock
       (Test_Portfolio.add_stock Test_Portfolio.empty_portfolio "AAPL" 10)
       "AAPL" 7)
    "AAPL" 2

let remove_stock_test_portfolio6 =
  Test_Portfolio.remove_stock
    (Test_Portfolio.add_stock
       (Test_Portfolio.add_stock Test_Portfolio.empty_portfolio "AAPL" 10)
       "MSFT" 5)
    "AAPL" 2

let remove_stock_test_portfolio7 =
  Test_Portfolio.remove_stock
    (Test_Portfolio.remove_stock
       (Test_Portfolio.add_stock
          (Test_Portfolio.remove_stock
             (Test_Portfolio.add_stock
                (Test_Portfolio.add_stock Test_Portfolio.empty_portfolio "AAPL"
                   10)
                "MSFT" 5)
             "AAPL" 2)
          "GOOG" 12)
       "AAPL" 8)
    "MSFT" 4

let remove_stock_tests =
  [
    ( "Removing a stock s from an portfolio completely" >:: fun _ ->
      assert_equal false
        (Test_Portfolio.contains_stock
           (Test_Portfolio.remove_stock remove_stock_test_portfolio1 "AAPL" 10)
           "AAPL") );
    ( " Removing a stock s from a portfolio non-completely " >:: fun _ ->
      assert_equal 3
        (Test_Portfolio.quantity_stock remove_stock_test_portfolio4 "AAPL") );
    ( " Removing a stock s multiple times from portfolio non-completely "
    >:: fun _ ->
      assert_equal 1
        (Test_Portfolio.quantity_stock remove_stock_test_portfolio5 "AAPL") );
    ( " Removing a stock s not completely from a non-empty portfolio - \
       presence of s"
    >:: fun _ ->
      assert_equal true
        (Test_Portfolio.contains_stock remove_stock_test_portfolio2 "MSFT") );
    ( " Removing a stock s not completely from a non-empty portfolio - \
       quantity of s. "
    >:: fun _ ->
      assert_equal 3
        (Test_Portfolio.quantity_stock remove_stock_test_portfolio2 "MSFT") );
    ( " Removing a stock s completely from a non-empty portfolio - quantity of \
       s."
    >:: fun _ ->
      assert_equal 0
        (Test_Portfolio.quantity_stock remove_stock_test_portfolio3 "MSFT") );
    ( " Removing a stock s completely from a non-empty portfolio - presence of \
       s. "
    >:: fun _ ->
      assert_equal false
        (Test_Portfolio.contains_stock remove_stock_test_portfolio3 "MSFT") );
    ( "Removing a share from a portfolio containing multiple tickers checking \
       ticker quantity of stock not removed"
    >:: fun _ ->
      assert_equal 5
        (Test_Portfolio.quantity_stock remove_stock_test_portfolio6 "MSFT") );
    ( "Removing a share from a portfolio containing multiple tickers checking \
       ticker quantity of stock removed"
    >:: fun _ ->
      assert_equal 8
        (Test_Portfolio.quantity_stock remove_stock_test_portfolio6 "AAPL") );
    ( "Test of add/remove sequence test" >:: fun _ ->
      assert_equal 0
        (Test_Portfolio.quantity_stock remove_stock_test_portfolio7 "AAPL") );
    ( "Test of add/remove sequence test" >:: fun _ ->
      assert_equal 1
        (Test_Portfolio.quantity_stock remove_stock_test_portfolio7 "MSFT") );
    ( "Test of add/remove sequence test" >:: fun _ ->
      assert_equal 12
        (Test_Portfolio.quantity_stock remove_stock_test_portfolio7 "GOOG") );
  ]

(* Portfolios used to test [batches_data].*)
let batches_data_test_portfolio1 = Test_Portfolio.empty_portfolio

let batches_data_test_portfolio2 =
  Test_Portfolio.add_stock Test_Portfolio.empty_portfolio "AAPL" 10

let batches_data_test_portfolio3 =
  Test_Portfolio.remove_stock batches_data_test_portfolio2 "AAPL" 5

let batches_data_tests =
  [
    ( "Buy batch for empty portfolio." >:: fun _ ->
      assert_equal []
        (Test_Portfolio.batches_data batches_data_test_portfolio1 "buy" "MSFT")
    );
    ( "Sell batch for empty portfolio." >:: fun _ ->
      assert_equal []
        (Test_Portfolio.batches_data batches_data_test_portfolio1 "sell" "MSFT")
    );
    (* Only test the quantity because the price depends on the time the buy
       order was executed, which we don't have access to.*)

    (* TODO: Factor out code into a single helper.*)
    ( "Single-element buy batch for present stock in single-stock portfolio."
    >:: fun _ ->
      let batches =
        Test_Portfolio.batches_data batches_data_test_portfolio2 "buy" "AAPL"
      in
      (* Get the lone buy order's data.*)
      let data = List.nth batches 0 in
      (* Get the quantity of stock in that buy order.*)
      let qty =
        match data with
        | _, q, _ -> q
      in

      assert_equal 10 qty );
    ( "Single-element sell batch for present stock in single-stock portfolio."
    >:: fun _ ->
      let batches =
        Test_Portfolio.batches_data batches_data_test_portfolio3 "sell" "AAPL"
      in
      let data = List.nth batches 0 in
      let qty =
        match data with
        | _, q, _ -> q
      in

      assert_equal 5 qty );
    ( "Buy batch for absent stock in single-stock portfolio." >:: fun _ ->
      assert_equal []
        (Test_Portfolio.batches_data batches_data_test_portfolio2 "buy" "MSFT")
    );
    ( " Sell batch for absent stock in single-stock portfolio." >:: fun _ ->
      assert_equal []
        (Test_Portfolio.batches_data batches_data_test_portfolio3 "sell" "META")
    );
  ]

(* File and portfolios used in testing [Filesys.update_file],
   [Filesys.to_user_portfolio]. *)
let file = "data_dir/data.txt"

let filesys_test_portfolio1 =
  Test_Portfolio.add_stock Test_Portfolio.empty_portfolio "MSFT" 100

let filesys_test_portfolio2 =
  Test_Portfolio.add_stock filesys_test_portfolio1 "AAPL" 55

(* These tests all work with [CS-3110-Final-Project---zaz/data_dir/data.txt]*)
let filesys_tests =
  [
    (* It was tested interactively that an empty portfolio is correctly
       converted to a data file. *)
    (* ( " Convert an empty portfolio to a data file and back. " >:: fun _ ->
       assert_equal ~printer:pp_portfolio Test_Portfolio.empty_portfolio
       (Test_Portfolio.empty_portfolio |> Test_Filesys.update_file file |>
       Test_Filesys.to_user_portfolio) ); *)
    ( " Convert a non-empty portfolio to a data file and back. " >:: fun _ ->
      assert_equal filesys_test_portfolio1
        (ignore (Test_Filesys.update_file file filesys_test_portfolio1);
         Test_Filesys.to_user_portfolio file) );
    (* TODO: Add more complex tests. Possible concern: commented-out case below
       was taking too long to run.*)

    (* ( " Convert non-empty data file to portfolio, add stock, update data
       file, \ reconvert to portfolio. " >:: fun _ -> assert_equal
       (Test_Portfolio.display_portfolio filesys_test_portfolio2) (let p =
       Test_Filesys.to_user_portfolio file in let p' = Test_Portfolio.add_stock
       p "AAPL" 55 in ignore (Test_Filesys.update_file file p');
       Test_Filesys.to_user_portfolio file |> Test_Portfolio.display_portfolio)
       ); *)
  ]

let api_tests =
  [
    (* Testing for determinable functions *)
    ( "get_yr_m_d test1" >:: fun _ ->
      assert_equal "1970-01-01" (Api.get_yr_m_d (Unix.localtime 40600.)) );
    ( "get_yr_m_d test2" >:: fun _ ->
      assert_equal "2023-12-09" (Api.get_yr_m_d (Unix.localtime 1702108888.)) );
    ( "get_hr_min test1" >:: fun _ ->
      assert_equal "06:07" (Api.get_hr_min (Unix.localtime 40060.)) );
    ( "get_hr_min test2" >:: fun _ ->
      assert_equal "03:09" (Api.get_hr_min (Unix.localtime 1702109368.)) );
    ( "get_time test1" >:: fun _ ->
      assert_equal "1970-01-01 06:06" (Api.get_time (Unix.localtime 40000.)) );
    ( "get_time test2" >:: fun _ ->
      assert_equal "2023-12-09 01:06"
        (Api.get_time (Unix.localtime 1702101971.)) );
  ]

let cost_basis_tests =
  [ (* ("cost basis test 1" >:: fun _ -> assert_equal (Some (Api.get_price
       "MSFT")) (Test_Portfolio.cost_basis add_stock_test_portfolio_1
       "MSFT")); *) ]

let suite =
  "test suite for portfolio.ml, filesys.ml"
  >::: List.flatten
         [
           contains_stock_tests;
           add_stock_tests;
           remove_stock_tests;
           batches_data_tests;
           api_tests;
           cost_basis_tests;
           filesys_tests;
         ]

let _ = run_test_tt_main suite
