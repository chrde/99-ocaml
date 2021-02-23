open OUnit2;;
open Lib;;

let tests = "test suite" >::: [
    "two" >:: (fun _ -> assert_equal (Some 2) (Lists.last [2]));
]

let _ = run_test_tt_main tests
