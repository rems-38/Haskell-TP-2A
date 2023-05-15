import System.IO
import System.IO.Strict
import System.Directory(doesFileExist)
import Data.Bool(bool)

import Json
import Tas
import Parser
import JSONParser

type File = Tas (Integer, Task)

-- Dans le fichier taches.txt, on va stocker la liste des tâches en attente,
-- chacune avec leur priorité, ie. une liste de paires (Integer, Task). Il faut
-- donc indiquer comment sérialiser des Integer et des paires.

instance Serializable Integer where
  serialize i = JSON_Int i
  deserialize (JSON_Int i) = Just i
  deserialize _ = Nothing

instance (Serializable a, Serializable b) => Serializable (a, b) where
  serialize (a, b) = JSON_Array [serialize a, serialize b]
  deserialize (JSON_Array [ja, jb]) = do
    a <- deserialize ja
    b <- deserialize jb
    Just (a, b)
  deserialize _ = Nothing

-- Chargement du fichier taches.txt dans une File. C'est un peu chevelu et vous
-- n'avez pas besoin de comprendre le code. (Mais vous avez tous les éléments
-- nécessaires !)

charger_file :: IO File
charger_file = do
  let path = "taches.txt"
  -- str = Nothing si le fichier n'existe pas, Just <contenus> s'il existe
  m_str <- doesFileExist path >>=
           bool (return Nothing) (Just <$> System.IO.Strict.readFile path)
  -- On parse le texte en JSON, on utilise fst pour garder que la valeur (le
  -- parser nous donne aussi le "reste du texte"), puis on désérialise le JSON
  -- en une liste de (Integer, Task)
  let taches = m_str >>= (\str -> (fst <$> pJSON str) >>= deserialize)
  -- Si on a un résultat on construit le tas, sinon erreur + tas vide
  case taches of
    Nothing -> do putStrLn "(taches.txt n'existe pas ou est invalide)"
                  return Vide
    Just taches -> return (construit taches)

-- Parser pour la commande a.
-- C'est très pratique d'avoir des parsers sous la main !

pTask :: Parser Task
pTask = pPrintSum <||> pPrintVal
  where pPrintVal = PrintVal <$$> pNombre
        pPrintSum str = do
          (x, str) <- pNombre str
          (y, str) <- (pCaractere '+' >>> pNombre) str
          Just (PrintSum x y, str)

decoder_tache :: String -> Maybe (Integer, Task)
decoder_tache str = do
  (priorite, str) <- pNombre str
  (task, str) <- pTask str
  Just (priorite, task)

-- Fonctions interactives.

interactif :: File -> IO File
interactif file = do
  putStrLn "Liste des tâches :"
  mapM afficher_prio_tache (deconstruit file)
  putStr "> "
  hFlush stdout
  saisie <- getLine
  (continuer, file) <- action file saisie
  if continuer then interactif file else return file

main :: IO ()
main = do
  putStrLn "Ordonnancement de tâches!"
  putStrLn "Commandes:"
  putStrLn "  e: Exécuter une tâche (s'il y en a)"
  putStrLn "  a <priorité> <valeur>: Ajouter une tâche PrintVal"
  putStrLn "  a <priorité> <valeur>+<valeur>: Ajouter une tâche PrintSum"
  putStrLn "  q: Quitter"
  file <- charger_file
  file <- interactif file
  sauver_file file

-- ========================================================================= --

afficher_prio_tache :: (Integer, Task) -> IO ()
afficher_prio_tache _ = return ()

executer_tache :: Task -> IO ()
executer_tache _ = return ()

action :: File -> String -> IO (Bool, File)
action _ _ = return (False, Vide)

sauver_file :: File -> IO ()
sauver_file _ = return ()
