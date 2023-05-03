{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Pics where
import Graphics.Svg as S

{- Dérivé d'un fichier fourni par P. Thiemann et G. Radanne
   http://proglang.informatik.uni-freiburg.de/teaching/functional-programming/2019/ -}

-- Les coordonnées sont de la forme (x, y)
type Coord = (Float, Float)

data Picture =
    Line Coord Coord
  | Circle {center::Coord, radius::Float}
  | Combine [Picture]
  deriving (Eq, Show)

-- Fournit l'opérateur "<>" pour combiner des images
instance Semigroup Picture where
  a <> b = Combine [a, b]

-- Fournit la constante "mempty" pour représenter une image vide
instance Monoid Picture where
  mempty = Combine []

{- ### Fonctions de dessin ### -}

-- line xy₁ xy₂ : Ligne entre deux points
line = Line

-- circle xy rayon : Cercle de centre xy
circle = Circle

-- polyline [xy₁, xy₂, ... xyₙ] : Série de lignes
polyline (x:y:t) = line x y <> polyline (y:t)
polyline _ = mempty

-- rect xy largeur hauteur : Rectangle (xy est le coin haut gauche)
rect (x, y) len height =
  polyline [ (x, y), (x+len, y), (x+len, y+height), (x, y+height), (x, y) ]
  
{- ### Opérations sur les coordonnées ### -}

negC (x, y) = (-x, -y)
(+/) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
(*/) (x, y) q = (x * q, y * q)
(°/) (x, y) th = (x * cos th - y * sin th, x * sin th + y * cos th)

{- ### Opérations sur les images ### -}

mapPic f g pic = case pic of
  Line a b -> Line (f a) (f b)
  Circle center radius -> Circle (f center) (g radius)
  Combine l -> Combine (map (mapPic f g) l)

-- moveP xy image : Translate une image dans la direction xy
moveP v = mapPic (+/ v) id
-- scaleP facteur image : Agrandit/rétrécit une image du facteur donné
scaleP q = mapPic (*/ q) (* q)
-- rotate0 theta image : Rotation de theta radians autour de (0, 0)
rotate0 theta = mapPic (°/ theta) id
-- rotateP theta xy image : Rotation de theta radians autour de xy
rotateP theta center = moveP center . rotate0 theta . moveP (negC center)

{- ### Exemples d'images ### -}

triangle = polyline [(0, 100), (100, 100), (50, 0), (0, 100)]

basic :: Picture
basic = rect (0, 0) 100 100 <> triangle

house :: Picture
house =
  rect (0, 0) 100 (-100)
  <> line (0, 0) (100, -100)
  <> line (100, 0) (0, -100)
  <> moveP (0, -200) triangle
  <> moveP (0, -200) (circle (50, 70) 25)

mkLandscape l =
  foldr f mempty l
  where
    f q pic = scaleP q house <> moveP (q*100, 0) pic

landscape = mkLandscape [0.5, 0.7, 1, 0.9, 0.3, 0.8]

{- ### Export SVG ### -}

toSvg :: Picture -> S.Element
toSvg pic = case pic of
  Line (x1, y1) (x2, y2) -> path_ [D_ <<- mA x1 y1 <> lA x2 y2]
  Circle {center = (cx, cy), radius} ->
    circle_ [Cx_ <<- (toText cx),
             Cy_ <<- (toText cy),
             R_ <<- toText radius]
  Combine l -> mconcat (map toSvg l)

-- output fichier image : Génère un fichier .svg pour une image
-- Exemple : output "landscape.svg" lanscape
output :: String -> Picture -> IO ()
output path pic =
  let svg = doctype <>
            with (svg11_ (toSvg (moveP (0, 200) pic)))
              [Stroke_ <<- "black", Fill_ <<- "none",
           Width_ <<- "1200", Height_ <<- "980"] in
  renderToFile path svg

