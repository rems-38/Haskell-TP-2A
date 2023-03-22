data JSON =
    JSON_Int Integer |
    JSON_Bool Bool |
    JSON_String String |
    JSON_Array [JSON] |
    JSON_Object [(String, JSON)]

exemple_tableau :: JSON
exemple_tableau = JSON_Array [JSON_Int 42, JSON_Bool True, JSON_String "JSON!", JSON_Array [JSON_Bool False, JSON_Int 0]]

exemple_objet :: JSON
exemple_objet = JSON_Object [("entier", JSON_Int 42), ("texte", JSON_String "Haskell!"), ("tableau", JSON_Array [JSON_Bool True])]
