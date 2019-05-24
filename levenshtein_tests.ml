open OUnit2
open Levenshtein

let l = levenshtein2 10 10 10 1 1
   
let test_empty_strings test_ctxt =
  assert_equal 0 (l "" "")

let test_skip_one_character test_ctxt =
  assert_equal 1 (l "a" "ba")
  
let suite =
  "suite" >:::
    [
(* begin suite *)
  "test_empty_strings" >:: test_empty_strings;
  "test_skip_one_character" >:: test_skip_one_character;
(* end suite *)
    ]

let () =
  run_test_tt_main suite
    
