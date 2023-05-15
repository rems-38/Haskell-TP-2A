module Parser where
import Data.Char

-- Un parser est une fonction qui lit une valeur au début d'une chaîne de
-- caractères. Par exemple le parser "nombre entier" lira le 42 au début de la
-- chaîne "42, 73" et renverra l'entier 42 accompagné du reste ", 73" sous la
-- forme Just (42, ", 73"). Si l'entrée n'est pas conforme (ie. ne commence
-- pas par un entier), il renverra Nothing.
type Parser a = String -> Maybe (a, String)

-- Comme exemple de base, ce parser lit un chiffre. Si la chaîne commence par
-- un chiffre, il l'accepte et le renvoie :
--   pChiffre "2b-M"   -->   Just ('2', "b-M")
-- Mais si la chaine ne commence pas par un chiffre, il la rejette :
--   pChiffre "Hello"   -->   Nothing
pChiffre :: Parser Char
pChiffre "" = Nothing
pChiffre (c:str)
  | isDigit c = Just (c, str)
  | otherwise = Nothing

-- Pour se faciliter la vie on va sauter les espaces au début de la chaîne dans
-- tous les parsers élémentaires (ceux qui inspectent la chaîne directement).
-- Comme ça on ne se souciera pas des espaces.
sauterEspaces :: String -> String
sauterEspaces = dropWhile isSpace

-- Lit un nombre entier :
--   pNombre "-42 reste"   -->   Just (-42, " reste")
pNombre :: Parser Integer
pNombre str =
  -- On cherche le signe s'il y en a un (remarquez qu'on saute les espaces)
  let (negatif, str2) = case sauterEspaces str of
                          '-' : str -> (True, str)
                          str -> (False, str) in
  -- Ensuite on extrait les chiffres et on convertit
  let (chiffres, str3) = span isDigit str2 in
  -- Pour que le nombre soit valide il faut qu'il y ait au moins un chiffre
  if chiffres == "" then
    Nothing
  else
    Just (if negatif then -read chiffres else read chiffres, str3)

-- Lit un caractère explicite (crochets, virgules, parenthèses...). Ce
-- mini-parser est utilisé pour la ponctuation. Il ne fait que consommer un
-- caractère et n'a pas de valeur à reconstruire donc on ne renvoie rien.
--   pCaractere '[' "[1,2,3]"   -->   Just ((), "1,2,3]")
--   pCaractere '{' "[1,2,3]"   -->   Nothing
pCaractere :: Char -> Parser ()
pCaractere c str = aux (sauterEspaces str)
  where aux (c':str)
          | c' == c = Just ((), str)
          | otherwise = Nothing
        aux "" = Nothing

-- Lit une chaîne de caractères entre guillemets
--   pChaine "\"abc\" reste"   -->   Just ("abc", " reste")
pChaine :: Parser String
pChaine str =
  case sauterEspaces str of
    '"' : str ->
      let (quoted, str2) = span (/= '"') str in
      if str2 == "" then Nothing else Just (quoted, tail str2)
    _  -> Nothing

-- Opérateur utile pour enchaîner deux parsers. Le résultat du premier est
-- jeté. On s'en servira quand le premier c'est pCaractere.
(>>>) :: Parser a -> Parser b -> Parser b
(>>>) p1 p2 str = do
  (_, str') <- p1 str
  p2 str'

-- Opérateur utile pour transformer la valeur retournée. Si le parser renvoyait
-- Just (a, reste) alors on renvoie Just (f a, reste). S'il avait échoué et
-- renvoyé Nothing alors on renvoie toujours Nothing.
(<$$>) :: (a -> b) -> Parser a -> Parser b
(<$$>) f p str = (\(a, reste) -> (f a, reste)) <$> p str

-- On se rappelle que Maybe est une monade, du coup on peut utiliser la
-- notation do quand on veut enchaîner plusieurs parsers. Par exemple, le
-- parser suivant lit 3 entiers séparés par des virgules:
--   pTroisEntiers "1, 2, -3 reste"   -->   Just ((1, 2, -3), " reste")
-- Remarquez qu'à chaque appel on remplace `str` par son reste.
pTroisEntiers :: Parser (Integer, Integer, Integer)
pTroisEntiers str = do
  -- D'abord on lit un entier avec pNombre
  (entier1, str) <- pNombre str
  -- Ensuite on lit une virgule (pCaractere ',') suivie d'un autre entier
  (entier2, str) <- (pCaractere ',' >>> pNombre) str
  -- Et puis encore un troisième
  (entier3, str) <- (pCaractere ',' >>> pNombre) str
  -- Les erreurs sont propagées automatiquement par do (voir TP6) du coup on
  -- n'a besoin de gérer que le cas où ça se passe bien, pour lequel on renvoie
  -- un Just.
  Just ((entier1, entier2, entier3), str)

-- On peut transformer la valeur renvoyée par un parser avec <$$>.
-- Ce parser génère une liste de trois entiers au lieu d'un triplet :
--   pListeTroisEntiers "1, 2, -3 reste"   -->   Just ([1, 2, -3], " reste")
pListeTroisEntiers :: Parser [Integer]
pListeTroisEntiers = tripletVersListe <$$> pTroisEntiers
  where tripletVersListe (x, y, z) = [x, y, z]

-- Répète un parser jusqu'à ce qu'il échoue, et collecte les valeurs lues dans
-- une liste.
cRepeter :: Parser a -> Parser [a]
cRepeter p str = case p str of
  Just (a, str) -> ((a:) <$$> cRepeter p) str
  Nothing -> Just ([], str)

-- Si on ne connaît pas à l'avance la taille de la liste, on peut utiliser le
-- combinateur de répétition.
--   pListeEntiers "1 2 3 4 x"   -->   Just ([1, 2, 3, 4], " x")
--   pListeEntiers "x"   -->   Just ([], "x")
pListeEntiers :: Parser [Integer]
pListeEntiers = cRepeter pNombre
