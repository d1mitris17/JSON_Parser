type json_value =
  | JsonObject of (string * json_value) list
  | JsonArray of json_value list
  | JsonString of string
  | JsonNumber of float
  | JsonBool of bool
  | JsonNull
