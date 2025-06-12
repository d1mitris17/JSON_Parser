open Ast

exception LexerError of string

let tokenise input =
  let len = String.length input in
  let pos = ref 0 in
  let tokens = ref [] in

  let rec skip_whitespace () =
    if
      !pos < len
      && (input.[!pos] = ' '
         || input.[!pos] = '\t'
         || input.[!pos] = '\n'
         || input.[!pos] = '\r')
    then (
      incr pos;
      skip_whitespace ())
  in
  let consume_str () =
    incr pos;
    (* Skip opening quote *)
    let buffer = Buffer.create 16 in
    while !pos < len && input.[!pos] <> '"' do
      if input.[!pos] = '\\' then (
        incr pos;
        if !pos < len then
          match input.[!pos] with
          | '"' -> Buffer.add_char buffer '"'
          | 'n' -> Buffer.add_char buffer '\n'
          | 't' -> Buffer.add_char buffer '\t'
          | '\\' -> Buffer.add_char buffer '\\'
          | c -> Buffer.add_char buffer c
        else raise (LexerError "Unexpected end after backslash"))
      else Buffer.add_char buffer input.[!pos];
      incr pos
    done;
    if !pos >= len || input.[!pos] <> '"' then
      raise (LexerError "Unterminated string literal");
    incr pos;
    (* Skip closing quote *)
    String (Buffer.contents buffer)
  in

  let consume_number () =
    let start_pos = !pos in
    if input.[!pos] = '-' then incr pos;

    while !pos < len && input.[!pos] >= '0' && input.[!pos] <= '9' do
      incr pos
    done;

    if !pos < len && input.[!pos] = '.' then (
      incr pos;
      while !pos < len && input.[!pos] >= '0' && input.[!pos] <= '9' do
        incr pos
      done);

    if !pos < len && (input.[!pos] = 'e' || input.[!pos] = 'E') then (
      incr pos;
      if !pos < len && (input.[!pos] = '+' || input.[!pos] = '-') then incr pos;
      while !pos < len && input.[!pos] >= '0' && input.[!pos] <= '9' do
        incr pos
      done);

    let num_str = String.sub input start_pos (!pos - start_pos) in
    try Number (float_of_string num_str)
    with Failure _ -> raise (LexerError ("Invalid number: " ^ num_str))
  in
  while !pos < len do
    skip_whitespace ();
    if !pos >= len then ()
    else
      match input.[!pos] with
      | '{' ->
          tokens := LBrace :: !tokens;
          pos := !pos + 1
      | '}' ->
          tokens := RBrace :: !tokens;
          pos := !pos + 1
      | '[' ->
          tokens := LBracket :: !tokens;
          pos := !pos + 1
      | ']' ->
          tokens := RBracket :: !tokens;
          pos := !pos + 1
      | ':' ->
          tokens := Colon :: !tokens;
          pos := !pos + 1
      | ',' ->
          tokens := Comma :: !tokens;
          pos := !pos + 1
      | 'f' when String.sub input !pos 4 = "false" ->
          tokens := Bool false :: !tokens;
          pos := !pos + 5
      | 't' when String.sub input !pos 3 = "true" ->
          tokens := Bool true :: !tokens;
          pos := !pos + 1
      | 'n' when String.sub input !pos 4 = "null" ->
          tokens := Null :: !tokens;
          pos := !pos + 1
      | c when (c >= '0' && c <= '9') || c = '-' ->
          let int_token = consume_number () in
          tokens := int_token :: !tokens
      | x when x = '"' ->
          let str_token = consume_str () in
          tokens := str_token :: !tokens
      | _ ->
          raise
            (LexerError ("Unexpected character: " ^ String.make 1 input.[!pos]))
  done;
  List.rev !tokens
