import Data.List (intercalate)

data JSON =
    JSON_Int Integer |
    JSON_Bool Bool |
    JSON_String String |
    JSON_Array [JSON] |
    JSON_Object [(String, JSON)] deriving Eq

exemple_tableau :: JSON
exemple_tableau = JSON_Array [JSON_Int 42, JSON_Bool True, JSON_String "JSON!", JSON_Array [JSON_Bool False, JSON_Int 0]]

exemple_objet :: JSON
exemple_objet = JSON_Object [("entier", JSON_Int 42), ("texte", JSON_String "Haskell!"), ("tableau", JSON_Array [JSON_Bool True])]

show_json :: JSON -> String
show_json (JSON_Int n) = show n
show_json (JSON_Bool b) = show b
show_json (JSON_String s) = show s
show_json (JSON_Array []) = "[]"
show_json (JSON_Array xs) = "[" ++ (intercalate ", " (map show_json xs)) ++ "]"
show_json (JSON_Object []) = "{}"
show_json (JSON_Object xs) = "{" ++ (intercalate ", " (map show_pair xs)) ++ "}"
  where
    show_pair (k, v) = show k ++ ": " ++ show_json v

instance Show JSON where
    show = show_json

-- Bonus/personal function to get "hello" and not "\"hello\""
print_json :: JSON -> IO()
print_json json = putStrLn (show_json json)

data Task = PrintVal Integer | PrintSum Integer Integer deriving Show

serialize_task :: Task -> JSON
serialize_task (PrintVal x) = JSON_Object [("op", JSON_String "PrintVal"), ("x", JSON_Int x)]
serialize_task (PrintSum x y) = JSON_Object [("op", JSON_String "PrintSum"), ("x", JSON_Int x), ("y", JSON_Int y)]

deserialize_task :: JSON -> Maybe Task
deserialize_task (JSON_Object obj) | (tail $ tail obj) == [] = Just $ PrintVal (jsonToInt (snd (head $ tail obj)))
                                   | otherwise = Just $ PrintSum (jsonToInt (snd (head $ tail obj))) (jsonToInt (snd (head $ tail $ tail obj)))
deserialize_task _ = Nothing

jsonToInt :: JSON -> Integer
jsonToInt (JSON_Int x) = x

