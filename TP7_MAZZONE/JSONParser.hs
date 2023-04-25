import Parser

pTabChar :: [Char] -> Parser ()
pTabChar [] str = Just ((), str)
pTabChar (c:cr) str = (pCaractere c >>> (pTabChar cr)) str

getReste :: Maybe (a, String) -> String
getReste Nothing = ""
getReste (Just (_, str)) = str

pBooleen :: Parser Bool
pBooleen str = case sauterEspaces str of
                't' : r -> if (pTabChar ['r', 'u', 'e'] r) /= Nothing
                            then Just (True, getReste (pTabChar ['r', 'u', 'e'] r))
                            else Nothing
                'f' : r -> if (pTabChar ['a', 'l', 's', 'e'] r) /= Nothing
                            then Just (False, getReste (pTabChar ['a', 'l', 's', 'e'] r))
                            else Nothing