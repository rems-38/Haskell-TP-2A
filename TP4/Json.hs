{-# LANGUAGE FlexibleInstances #-} -- pour que l'instance Serializable String fonctionne
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
deserialize_task (JSON_Object obj) | (tail $ tail obj) == [] = Just $ PrintVal x
                                   | otherwise = Just $ PrintSum x y
    where x = jsonToInt (snd (head $ tail obj))
          y = jsonToInt (snd (head $ tail $ tail obj))
deserialize_task _ = Nothing

jsonToInt :: JSON -> Integer
jsonToInt (JSON_Int x) = x

class Serializable a where
    serialize :: a -> JSON
    deserialize :: JSON -> Maybe a

instance Serializable Task where
    serialize = serialize_task
    deserialize = deserialize_task

instance (Serializable a) => Serializable [a] where
    serialize = serialize_list
    deserialize = deserialize_list

serialize_list :: Serializable a => [a] -> JSON
serialize_list xs = JSON_Array $ map serialize xs

deserialize_list :: Serializable a => JSON -> Maybe [a]
deserialize_list (JSON_Array arr) = Just $ map unMaybe (map deserialize arr)
deserialize_list _ = Nothing

unMaybe :: Maybe a -> a 
unMaybe (Just x) = x

instance Serializable Integer where
    serialize n = JSON_Int $ n
    deserialize (JSON_Int n) = Just n
    deserialize _ = Nothing

instance Serializable Bool where
    serialize b = JSON_Bool $ b
    deserialize (JSON_Bool b) = Just b
    deserialize _ = Nothing

instance {-# OVERLAPPING #-} Serializable String where
    serialize s = JSON_String $ s
    deserialize (JSON_String s) = Just s
    deserialize _ = Nothing
