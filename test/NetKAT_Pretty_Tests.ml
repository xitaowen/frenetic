open Types
open Pretty
open VInt
module SDN = SDN_Types

let policy_parse (p : string) : Types.policy =
  Parser.program Lexer.token (Lexing.from_string p)

let test_pretty str =
  let pol1 = Parser.program Lexer.token (Lexing.from_string str) in
  let str' = string_of_policy pol1 in
  let pol2 = Parser.program Lexer.token (Lexing.from_string str') in
  pol1 = pol2

let str1 = "filter port = 1"
let str2 = "filter port = 1 ; port := 2"
let str3 = "filter switch = 1; filter port = 1 ; port := 2 |
            (filter switch = 1 ; filter port = 2 ; port := 1 |
             filter switch = 2 ; filter port = 1 ; port := 2)"

TEST "simple filter" = test_pretty str1 = true
TEST "simple sequence" = test_pretty str2 = true
TEST "assoc par" = test_pretty str3 = true

let testable_pol_to_bool = 
  let open QuickCheck in
  let open QuickCheck_gen in
  let open NetKAT_Arbitrary in
  testable_fun 
    (resize 11 arbitrary_policy) 
    string_of_policy testable_bool

let prop_parse_pol_idempotent (p : Types.policy) : bool =
  try policy_parse (string_of_policy p) = p
  with _ -> 
    Printf.printf "POL: %s\n" (string_of_policy p);
    assert false

TEST "testing parse-pretty roundtrip" =
  let cfg = { QuickCheck.quick with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_to_bool cfg prop_parse_pol_idempotent with
    | QuickCheck.Success -> true
    | _ -> failwith "quickchecking failed"
