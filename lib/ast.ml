type json_value =
  | JsonObject of (string * json_value) list
  | JsonArray of json_value list
  | JsonString of string
  | JsonNumber of float
  | JsonBool of bool
  | JsonNull

type token =
  | LBrace (* { *)
  | RBrace (* } *)
  | LBracket (* [ *)
  | RBracket (* ] *)
  | Colon (* : *)
  | Comma (* , *)
  | String of string
  | Number of float
  | Bool of bool
  | Null
  | EOF

let string_of_token token =
  match token with
  | LBrace -> "{"
  | RBrace -> "}"
  | LBracket -> "["
  | RBracket -> "]"
  | Colon -> ":"
  | Comma -> ","
  | String s -> Printf.sprintf "\"%s\"" s
  | Number n -> string_of_float n
  | Bool b -> string_of_bool b
  | Null -> "null"
  | EOF -> "EOF"

let print_token token = print_endline (string_of_token token)
let print_token_list tokens = List.iter (fun tok -> print_token tok) tokens
