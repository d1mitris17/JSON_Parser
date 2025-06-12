open Json_parser.Ast
open Json_parser.Parser

(* Helper function to compare JSON values *)
let rec json_equal j1 j2 = match (j1, j2) with
  | (JNull, JNull) -> true
  | (JBool b1, JBool b2) -> b1 = b2
  | (JNumber n1, JNumber n2) -> n1 = n2
  | (JString s1, JString s2) -> s1 = s2
  | (JArray l1, JArray l2) -> 
    List.length l1 = List.length l2 && List.for_all2 json_equal l1 l2
  | (JObject pairs1, JObject pairs2) ->
    List.length pairs1 = List.length pairs2 &&
    List.for_all2 (fun (k1, v1) (k2, v2) -> k1 = k2 && json_equal v1 v2) pairs1 pairs2
  | _ -> false

(* Test 1: Simple values *)
let test_simple_values () =
  let tests = [
    ([Null; EOF], JNull);
    ([Bool true; EOF], JBool true);
    ([Bool false; EOF], JBool false);
    ([Number 42.0; EOF], JNumber 42.0);
    ([String "hello"; EOF], JString "hello");
  ] in
  List.iter (fun (tokens, expected) ->
    let actual = parse_tokens tokens in
    assert (json_equal expected actual)
  ) tests;
  Printf.printf "✓ Simple values test passed\n"

(* Test 2: Empty containers *)
let test_empty_containers () =
  let tests = [
    ([LBracket; RBracket; EOF], JArray []);
    ([LBrace; RBrace; EOF], JObject []);
  ] in
  List.iter (fun (tokens, expected) ->
    let actual = parse_tokens tokens in
    assert (json_equal expected actual)
  ) tests;
  Printf.printf "✓ Empty containers test passed\n"

(* Test 3: Simple arrays *)
let test_simple_arrays () =
  let tokens = [LBracket; Number 1.0; Comma; Number 2.0; Comma; Number 3.0; RBracket; EOF] in
  let expected = JArray [JNumber 1.0; JNumber 2.0; JNumber 3.0] in
  let actual = parse_tokens tokens in
  assert (json_equal expected actual);
  Printf.printf "✓ Simple arrays test passed\n"

(* Test 4: Simple objects *)
let test_simple_objects () =
  let tokens = [LBrace; String "name"; Colon; String "Alice"; RBrace; EOF] in
  let expected = JObject [("name", JString "Alice")] in
  let actual = parse_tokens tokens in
  assert (json_equal expected actual);
  Printf.printf "✓ Simple objects test passed\n"

(* Test 5: Multiple object pairs *)
let test_multiple_object_pairs () =
  let tokens = [
    LBrace; 
    String "name"; Colon; String "Bob"; Comma;
    String "age"; Colon; Number 30.0;
    RBrace; EOF
  ] in
  let expected = JObject [("name", JString "Bob"); ("age", JNumber 30.0)] in
  let actual = parse_tokens tokens in
  assert (json_equal expected actual);
  Printf.printf "✓ Multiple object pairs test passed\n"

(* Test 6: Nested structures *)
let test_nested_structures () =
  let tokens = [
    LBrace;
    String "user"; Colon; LBrace; String "name"; Colon; String "Charlie"; RBrace;
    RBrace; EOF
  ] in
  let expected = JObject [("user", JObject [("name", JString "Charlie")])] in
  let actual = parse_tokens tokens in
  assert (json_equal expected actual);
  Printf.printf "✓ Nested structures test passed\n"

(* Test 7: Array of objects *)
let test_array_of_objects () =
  let tokens = [
    LBracket;
    LBrace; String "id"; Colon; Number 1.0; RBrace; Comma;
    LBrace; String "id"; Colon; Number 2.0; RBrace;
    RBracket; EOF
  ] in
  let expected = JArray [
    JObject [("id", JNumber 1.0)];
    JObject [("id", JNumber 2.0)]
  ] in
  let actual = parse_tokens tokens in
  assert (json_equal expected actual);
  Printf.printf "✓ Array of objects test passed\n"

(* Run all parser tests *)
let run_parser_tests () =
  Printf.printf "Running Parser Tests:\n";
  test_simple_values ();
  test_empty_containers ();
  test_simple_arrays ();
  test_simple_objects ();
  test_multiple_object_pairs ();
  test_nested_structures ();
  test_array_of_objects ();
  Printf.printf "All parser tests passed!\n\n
