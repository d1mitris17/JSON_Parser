open Json_parser.Ast
open Json_parser.Lexer

let rec token_lists_equal l1 l2 =
  match (l1, l2) with
  | [], [] -> true
  | h1 :: t1, h2 :: t2 -> (
      match (h1, h2) with
      | String s1, String s2 -> s1 = s2 && token_lists_equal t1 t2
      | Number n1, Number n2 -> n1 = n2 && token_lists_equal t1 t2
      | Bool b1, Bool b2 -> b1 = b2 && token_lists_equal t1 t2
      | LBrace, LBrace
      | RBrace, RBrace
      | LBracket, LBracket
      | RBracket, RBracket
      | Comma, Comma
      | Colon, Colon
      | Null, Null
      | EOF, EOF ->
          token_lists_equal t1 t2
      | _ -> false)
  | _ -> false

(* Test 1: Basic tokens *)
let test_basic_tokens () =
  let input = "{}" in
  let expected = [ LBrace; RBrace; EOF ] in
  let actual = tokenise input in
  assert (token_lists_equal expected actual);
  Printf.printf "✓ Basic tokens test passed\n"

(* Test 2: String literals *)
let test_string_literals () =
  let input = {|"hello world"|} in
  let expected = [ String "hello world"; EOF ] in
  let actual = tokenise input in
  assert (token_lists_equal expected actual);
  Printf.printf "✓ String literals test passed\n"

(* Test 3: Numbers *)
let test_numbers () =
  let tests =
    [
      ("123", [ Number 123.0; EOF ]);
      ("-45", [ Number (-45.0); EOF ]);
      ("12.34", [ Number 12.34; EOF ]);
      ("1.23e10", [ Number 1.23e10; EOF ]);
    ]
  in
  List.iter
    (fun (input, expected) ->
      let actual = tokenise input in
      assert (token_lists_equal expected actual))
    tests;
  Printf.printf "✓ Numbers test passed\n"

(* Test 4: Keywords *)
let test_keywords () =
  let tests =
    [
      ("true", [ Bool true; EOF ]);
      ("false", [ Bool false; EOF ]);
      ("null", [ Null; EOF ]);
    ]
  in
  List.iter
    (fun (input, expected) ->
      let actual = tokenise input in
      assert (token_lists_equal expected actual))
    tests;
  Printf.printf "✓ Keywords test passed\n"

(* Test 5: Complex JSON *)
let test_complex_tokenization () =
  let input = {|{"name": "Alice", "age": 25}|} in
  let expected =
    [
      LBrace;
      String "name";
      Colon;
      String "Alice";
      Comma;
      String "age";
      Colon;
      Number 25.0;
      RBrace;
      EOF;
    ]
  in
  let actual = tokenise input in
  assert (token_lists_equal expected actual);
  Printf.printf "✓ Complex tokenization test passed\n"

(* Test 6: Whitespace handling *)
let test_whitespace () =
  let input = {|  {  "key"  :  "value"  }  |} in
  let expected = [ LBrace; String "key"; Colon; String "value"; RBrace; EOF ] in
  let actual = tokenise input in
  assert (token_lists_equal expected actual);
  Printf.printf "✓ Whitespace handling test passed\n"

(* Test 7: Escape sequences *)
let test_escape_sequences () =
  let input = {|"hello\nworld\t\"quoted\""|} in
  let expected = [ String "hello\nworld\t\"quoted\""; EOF ] in
  let actual = tokenise input in
  assert (token_lists_equal expected actual);
  Printf.printf "✓ Escape sequences test passed\n"

(* Run all lexer tests *)
let run_lexer_tests () =
  Printf.printf "Running Lexer Tests:\n";
  test_basic_tokens ();
  test_string_literals ();
  test_numbers ();
  test_keywords ();
  test_complex_tokenization ();
  test_whitespace ();
  test_escape_sequences ();
  Printf.printf "All lexer tests passed!\n\n"

let () = run_lexer_tests ()
