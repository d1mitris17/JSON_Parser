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
