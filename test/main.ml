(* Test Plan.*)

(* 1. Parts of system automatically tested by OUnit. *)

(*- [portfolio.ml]: All the functions in the [.mli], except for those that
  return output containing information on dates/prices. These excluded functions
  are [display_portfolio], [display_portfolio_filesys]. The reason for these
  exclusions is the dates/prices depend on the current time the user polls the
  API. so these data are not known ahead of time. Black-box testing utilized
  throughout. *)

(* [filesys.ml]: For all the functions in the [.mli], the simplest test cases
   are provided. More complicated test cases were attempted, but they were
   taking too long to run, a possible concern. However, throughout interactive
   testing, the file-saving system did not hang unexpectedly (after additional
   fixes were made on the UI end). These simplest test cases utilized black-box
   testing. *)

(* 2. Parts of the system manually tested. *)

(* - [portfolio.ml]: [display_portfolio], [display_portfolio_filesys]. These
   were tested relatively extensively interactively, via utop and with the
   UI. *)
(* - [filesys.ml]: All the functions in the [.mli] file, and therefore their
   helpers, were tested interactively via utop continuously throughout
   development. *)

(*3. Why testing demonstrates the correctness of the system. *)

(* - Testing was done continuously throughout development, including interactive
   testing. *)
(* - The backend was tested independently of the front end, as well as
   together. *)

open OUnit2
open Stocks
open Portfolio
module Test_Portfolio = Portfolio.UserPortfolio
open Filesys
module Test_Filesys = FileSys

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
  ]

(* Portfolios used to test [remove_stock]. *)
let remove_stock_test_portfolio1 =
  Test_Portfolio.remove_stock Test_Portfolio.empty_portfolio "AAPL" 10

let remove_stock_test_portfolio2 =
  let p1 = Test_Portfolio.add_stock Test_Portfolio.empty_portfolio "MSFT" 5 in
  let p2 = Test_Portfolio.remove_stock p1 "MSFT" 2 in
  p2

let remove_stock_test_portfolio3 =
  Test_Portfolio.remove_stock remove_stock_test_portfolio2 "MSFT" 3

let remove_stock_test_portfolio4 =
  Test_Portfolio.remove_stock remove_stock_test_portfolio2 "AAPL" 10

let remove_stock_tests =
  [
    ( "Removing a stock s from an empty portfolio - presence of s. " >:: fun _ ->
      assert_equal false
        (Test_Portfolio.contains_stock remove_stock_test_portfolio1 "AAPL") );
    ( " Removing a stock s from an empty portfolio - quantity of s. "
    >:: fun _ ->
      assert_equal 0
        (Test_Portfolio.quantity_stock remove_stock_test_portfolio1 "AAPL") );
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
    ( "Removing a stock s that is absent from a non-empty portfolio - presence \
       of s. "
    >:: fun _ ->
      assert_equal false
        (Test_Portfolio.contains_stock remove_stock_test_portfolio4 "AAPL") );
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

(* TODO: possibly remove, since unimplemented. *)

(* Portfolios used to test [stock_price_over_time]. *)
let stock_price_over_time_test_portfolio1 = Test_Portfolio.empty_portfolio

let stock_price_over_time_test_portfolio2 =
  Test_Portfolio.add_stock Test_Portfolio.empty_portfolio "MSFT" 10

(* TODO: Run these tests in the suite.*)
let stock_price_over_time_tests =
  [
    (* To test this function: 1. Create a portfolio of stocks. 2. Get the
       initial buy date of one of the stocks to be tested (need a function for
       this). 3. Split the present time and initial buy time into 10 intervals
       (or whatever the number of intervals is) to get 10 times. 4. Ping API to
       get prices at those 10 times, build a list of those prices. 5. Compare
       function's output with 4.*)
    ( " Stock not in empty portfolio" >:: fun _ ->
      assert_equal []
        (Test_Portfolio.stock_price_over_time
           stock_price_over_time_test_portfolio1 "MSFT") );
    ( " Stock not in non-empty portfolio" >:: fun _ ->
      assert_equal []
        (Test_Portfolio.stock_price_over_time
           stock_price_over_time_test_portfolio2 "AAPL") );
    (* (" Stock in non-empty portfolio, long duration between present time and
       initial buy date. 10 data points.") (" Stock in non-empty portfolio,
       short duration between present time and initial buy date. 10 data
       points.") *)
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
    ( " Convert an empty portfolio to a data file and back. " >:: fun _ ->
      assert_equal Test_Portfolio.empty_portfolio
        (Test_Portfolio.empty_portfolio
        |> Test_Filesys.update_file file
        |> Test_Filesys.to_user_portfolio) );
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

let suite =
  "test suite for portfolio.ml, filesys.ml"
  >::: List.flatten
         [
           contains_stock_tests;
           add_stock_tests;
           remove_stock_tests;
           batches_data_tests;
           filesys_tests;
         ]

let _ = run_test_tt_main suite
