open OUnit2
open Stocks
open Portfolio
module Test_Portfolio = Portfolio.UserPortfolio

let contains_stock_tests =
  [
    ( "Empty portfolio" >:: fun _ ->
      assert_equal false
        (Test_Portfolio.contains_stock Test_Portfolio.empty_portfolio "FB") );
  ]

(* Portfolios used to test [add_stock]. *)
let add_stock_test_portfolio_1 =
  Test_Portfolio.add_stock Test_Portfolio.empty_portfolio "FB" 10

let add_stock_test_portfolio_2 =
  Test_Portfolio.add_stock add_stock_test_portfolio_1 "AAPL" 5

let add_stock_test_portfolio_3 =
  Test_Portfolio.add_stock add_stock_test_portfolio_2 "FB" 6

(* TODO: More comprehensive tests. *)
(* Also tests [quantity_stock] and [contains_stock]. *)
let add_stock_tests =
  [
    ( " Adding stock s to empty portfolio - presence of s. " >:: fun _ ->
      assert_equal true
        (Test_Portfolio.contains_stock add_stock_test_portfolio_1 "FB") );
    ( " Adding stock s to empty portfolio - quantity of s. " >:: fun _ ->
      assert_equal 10
        (Test_Portfolio.quantity_stock add_stock_test_portfolio_1 "FB") );
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
        (Test_Portfolio.quantity_stock add_stock_test_portfolio_3 "FB") );
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
  let p1 = Test_Portfolio.add_stock Test_Portfolio.empty_portfolio "FB" 5 in
  let p2 = Test_Portfolio.remove_stock p1 "FB" 2 in
  p2

let remove_stock_test_portfolio3 =
  Test_Portfolio.remove_stock remove_stock_test_portfolio2 "FB" 3

let remove_stock_test_portfolio4 =
  Test_Portfolio.remove_stock remove_stock_test_portfolio2 "AAPL" 10

(* TODO: More comprehensive tests. *)
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
        (Test_Portfolio.contains_stock remove_stock_test_portfolio2 "FB") );
    ( " Removing a stock s not completely from a non-empty portfolio - \
       quantity of s. "
    >:: fun _ ->
      assert_equal 3
        (Test_Portfolio.quantity_stock remove_stock_test_portfolio2 "FB") );
    ( " Removing a stock s completely from a non-empty portfolio - quantity of \
       s."
    >:: fun _ ->
      assert_equal 0
        (Test_Portfolio.quantity_stock remove_stock_test_portfolio3 "FB") );
    ( " Removing a stock s completely from a non-empty portfolio - presence of \
       s. "
    >:: fun _ ->
      assert_equal false
        (Test_Portfolio.contains_stock remove_stock_test_portfolio3 "FB") );
    ( "Removing a stock s that is absent from a non-empty portfolio - presence \
       of s. "
    >:: fun _ ->
      assert_equal false
        (Test_Portfolio.contains_stock remove_stock_test_portfolio4 "AAPL") );
  ]

let suite =
  "test suite for Portfolio.ml"
  >::: List.flatten
         [ contains_stock_tests; add_stock_tests; remove_stock_tests ]

let _ = run_test_tt_main suite
