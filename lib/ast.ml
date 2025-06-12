type json_value =
  | JsonObject of (string * json_value) list
  | JsonArray of json_value list
  | JsonString of string
  | JsonNumber of float
  | JsonBool of bool
  | JsonNull

type token =
  | LeftBrace (* { *)
  | RightBrace (* } *)
  | LeftBracket (* [ *)
  | RightBracket (* ] *)
  | Colon (* : *)
  | Comma (* , *)
  | String of string
  | Number of float
  | TRUE
  | FALSE
  | NULL
  | EOF
