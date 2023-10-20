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

let suite =
  "test suite for Portfolio.ml" >::: List.flatten [ contains_stock_tests ]

let _ = run_test_tt_main suite
