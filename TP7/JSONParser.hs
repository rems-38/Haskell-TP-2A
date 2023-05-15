{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Parser
import Data.Maybe ( isJust )

pTabChar :: [Char] -> Parser ()
pTabChar [] str = Just ((), str)
pTabChar (c:cr) str = (pCaractere c >>> pTabChar cr) str

getReste :: Maybe (a, String) -> String
getReste Nothing = ""
getReste (Just (_, str)) = str

pBooleen :: Parser Bool
pBooleen str = case sauterEspaces str of
                't' : r -> if isJust (pTabChar ['r', 'u', 'e'] r)
                            then Just (True, getReste (pTabChar ['r', 'u', 'e'] r))
                            else Nothing
                'f' : r -> if isJust (pTabChar ['a', 'l', 's', 'e'] r)
                            then Just (False, getReste (pTabChar ['a', 'l', 's', 'e'] r))
                            else Nothing

-- Combinateur "alternative"
(<||>) :: Eq a => Parser a -> Parser a -> Parser a
(<||>) p1 p2 str | isJust (p1 str) = p1 str
                 | otherwise = p2 str

-- problèmes dans cette fonction apparemment
cRepeterVirgules :: Eq a => Parser a -> Parser [a]
cRepeterVirgules p = cRepeter (pCaractere ',' >>> p <||> p)

data JSON =
    JSON_Int Integer |
    JSON_Bool Bool |
    JSON_String String |
    JSON_Array [JSON] |
    JSON_Object [(String, JSON)] deriving (Eq, Show)

pNombreJSON :: Parser JSON
pNombreJSON = JSON_Int <$$> pNombre

pBooleenJSON :: Parser JSON
pBooleenJSON = JSON_Bool <$$> pBooleen

pChaineJSON :: Parser JSON
pChaineJSON = JSON_String <$$> pChaine

(<<<) :: Parser b -> Parser a -> Parser b
p1 <<< p2 = p2 >>> p1

pTableauJSON :: Parser JSON
pTableauJSON = JSON_Array <$$> (pCaractere '[' >>> cRepeterVirgules pJSON <<< pCaractere ']')

-- Parseur d'un attribut JSON
pAttributJSON :: Parser (String, JSON)
pAttributJSON = do
                    cle <- pChaine
                    pCaractere ':'
                    val <- pJSON
                    case cle of
                        Just (c, _) -> return (c, val)
                        Nothing -> fail "Erreur de syntaxe : l'attribut doit être une chaîne de caractères"


pObjetJSON :: Parser JSON
pObjetJSON = JSON_Object <$$> (pCaractere '{' >>> cRepeterVirgules pAttributJSON <<< pCaractere '}')

pJSON :: Parser JSON
pJSON = pNombreJSON <||> pBooleenJSON <||> pChaineJSON <||> pTableauJSON <||> pObjetJSON

unMaybe :: Maybe a -> a
unMaybe (Just a) = a