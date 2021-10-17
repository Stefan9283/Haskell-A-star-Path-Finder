{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import qualified Data.Set as S


import ProblemState ( ProblemState(..) )
import Data.List
import Data.Maybe

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipurile Target și Game sunt definite mai jos.

    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}

data Game = Game {
    game_max :: Position,
    game_hunter :: Position,
    game_targets :: [Target],
    game_obstacles :: [Position],
    game_gateways :: [(Position, Position)]
} deriving (Eq, Ord)
{-
    *** Optional ***

    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}



{-
    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.

    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

gameAsString :: Game -> String
gameAsString game =
    replicate xm '@' ++ "\n" ++
    foldl
        (\acc pos ->
            case find (\x -> fst x == pos) symbols of
                        Nothing ->
                            if snd pos == 1 && snd pos == (xm - 2) then
                                acc ++ "@ @\n"
                            else if snd pos == 1 then
                                acc ++ "@ "
                            else if snd pos == (xm - 2) then
                                acc ++ " @\n"
                            else
                                acc ++ " "
                        Just p ->
                            if snd pos == 1 && snd pos == (xm - 2) then
                                acc ++ ['@', snd p, '@', '\n']
                            else if snd pos == 1 then
                                acc ++ ['@', snd p]
                            else if snd pos == (xm - 2) then
                                acc ++ [snd p, '@', '\n']
                            else
                                acc ++ [snd p])
        ""
        [(x, y) | x <- [1..(ym - 2)], y <- [1..(xm - 2)]] ++
    replicate xm '@'
    where
        xm = snd (game_max game)
        ym = fst (game_max game)
        symbols =
                    (game_hunter game, '!') :
                    foldl (\acc x -> (position x, '*') : acc) [] (game_targets game) ++
                    foldl (\acc x -> (x, '@') : acc) [] (game_obstacles game) ++
                    foldl (\acc x -> (snd x, '#') : (fst x, '#') : acc) [] (game_gateways game)


instance Show Game where
    show = gameAsString

{-
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame height width = Game (height, width) (1, 1) [] [] []

{-
    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.

-}
addHunter :: Position -> Game -> Game
addHunter pos game
    | fst (game_max game) <= fst pos || 0 >= fst pos || snd (game_max game) <= snd pos || 0 >= snd pos = game
    | game_hunter game == pos = game
    | pos `elem` foldl (\acc x -> position x : acc) [] (game_targets game) = game
    | pos `elem` game_obstacles game = game
    | otherwise = game{game_hunter = pos}

{-
    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget b pos game
    | Target pos b `elem` targets = game
    | otherwise = game{game_targets = Target pos b : targets}
    where
        targets = game_targets game


{-
    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway gateway game
    | gateway `elem` gates = game
    | (snd gateway, fst gateway) `elem` gates = game
    | otherwise = game{game_gateways = gateway : gates}
    where
        gates = game_gateways game

{-
    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos game
    | fst (game_max game) <= fst pos || 0 >= fst pos || snd (game_max game) <= snd pos || 0 >= snd pos = game
    | game_hunter game == pos = game
    | pos `elem` foldl (\acc x -> position x : acc) [] (game_targets game) = game
    | pos `elem` obstacles = game
    | otherwise = game{game_obstacles = pos : obstacles}
    where
        obstacles = game_obstacles game

{-
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove pos game
    | fst (game_max game) <= fst pos || 0 >= fst pos || snd (game_max game) <= snd pos || 0 >= snd pos = Nothing
    | pos `elem` game_obstacles game = Nothing
    | not(null gatewayAtPos) && fst (head gatewayAtPos) == pos = Just (snd (head gatewayAtPos))
    | not(null gatewayAtPos) && snd (head gatewayAtPos) == pos = Just (fst (head gatewayAtPos))
    | pos == game_hunter game = Nothing
    | otherwise = Just pos
    where
        gatewayAtPos = foldl (\acc x -> if fst x == pos || snd x == pos then x : acc else acc) [] (game_gateways game)


go :: Position -> Game -> Int -> Int -> Target
go pos game offx offy =
    case attemptMove (fst pos + offx, snd pos + offy) game of
        Just newPos -> t{position = newPos}
        Nothing ->
                if not(null gatewayAtPos) && fst (head gatewayAtPos) == pos then
                        t{position = snd (head gatewayAtPos)}
                else if not(null gatewayAtPos) && snd (head gatewayAtPos) == pos then
                        t{position = fst (head gatewayAtPos)}
                else t
    where
        t = fromJust (find (\target -> position target == pos) (game_targets game))
        gatewayAtPos = foldl (\acc x -> if fst x == pos || snd x == pos then x : acc else acc) [] (game_gateways game)

{-
    Comportamentul unui Target de a se deplasa cu o casuță înspre est.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.

    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.

    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}
goEast :: Behavior
goEast pos game = go pos game 0 1

{-
    Comportamentul unui Target de a se deplasa cu o casuță înspre vest.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.
-}
goWest :: Behavior
goWest pos game = go pos game 0 (-1)

{-
    Comportamentul unui Target de a se deplasa cu o casuță înspre nord.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.
-}
goNorth :: Behavior
goNorth pos game = go pos game (-1) 0

{-
    Comportamentul unui Target de a se deplasa cu o casuță înspre sud.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.
-}
goSouth :: Behavior
goSouth pos game = go pos game 1 0

{-
    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud.
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}



bounce :: Int -> Behavior
bounce dir pos game
  | after_target == initial_target =
        inverse_target{behavior = bounce (-dir)}
  | not(null gatewayAtPos) && (position after_target `elem` game_obstacles game ||
                    (dir ==  1 && fst (game_max game) - 1 == snd (position after_target)) ||
                    (dir == -1 &&                       1 == fst (position after_target)))
                      = after_target{position = (1, 1), behavior = bounce (-dir)} -- after_target{behavior = bounce (-dir)}
  | otherwise = after_target
  where
      initial_target = go pos game 0 0
      after_target = go pos game dir 0
      inverse_target = go pos game (- dir) 0
      gatewayAtPos
        = foldl
            (\ acc x -> if fst x == position after_target || snd x == position after_target then x : acc else acc)
            [] (game_gateways game)


{-
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
-}
moveTargets :: Game -> Game
moveTargets game =
    game{game_targets = foldl (\acc x -> behavior x (position x) game : acc) [] (game_targets game)}

{-
    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled pos target = hEuclidean pos (position target) <= 1


removedKilledTargets :: Game -> Game
removedKilledTargets game = game{game_targets = foldl (\acc x -> if isTargetKilled (game_hunter game) x then acc else x : acc) [] (game_targets game)}


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.

    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.

    advanceGameState South True $ advanceGameState South True $ advanceGameState South True testGame3
-}

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir mvTrgs game =
    case attemptMove (fst hunter + offx, snd hunter + offy) game of
        Just p ->
                    if mvTrgs then
                        let
                            movedHunter = removedKilledTargets game{game_hunter = p}
                            targetsMoved = moveTargets movedHunter
                        in removedKilledTargets targetsMoved
                    else game{game_hunter = p}
        Nothing -> if mvTrgs then
                        moveTargets game
                    else game
    where
        offx = fromEnum (dir == South) - fromEnum(dir == North)
        offy = fromEnum(dir == East) - fromEnum (dir == West)
        hunter = game_hunter game

{-
    ***  TODO ***
    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = length (game_targets game) > 1

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}

circle :: Position -> Int -> Behavior
circle cpos radius tpos game =
                case attemptMove (newtx, newty) game of
                    Just newPos -> target{position = newPos}
                    Nothing -> target
                where
                    target = fromJust (find (\t -> position t == tpos) (game_targets game))
                    theta = -2 * pi / (4.0 * fromIntegral radius) :: Float
                    x = fromIntegral (fst tpos - fst cpos)
                    y = fromIntegral (snd tpos - snd cpos)
                    newx = round  (x * cos theta - y * sin theta)
                    newy = round  (x * sin theta + y * cos theta)
                    newtx = newx + fst cpos
                    newty = newy + snd cpos






instance ProblemState Game Direction where
    {-
        *** TODO ***
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = let
                        gameS = (South, advanceGameState South False game)
                        gameN = (North, advanceGameState North False game)
                        gameW = (West, advanceGameState West False game)
                        gameE = (East, advanceGameState East False game)
                        statesList = [gameW, gameE, gameS, gameN]
                    in
                        foldl (\acc x -> if snd x == game || x `elem` acc then acc else x : acc) [] statesList

    {-
        *** TODO ***
        Verifică dacă starea curentă este una în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal game = not(null (foldl (\acc x -> if isTargetKilled (game_hunter game) x then x : acc else acc) [] (game_targets game)))

    {-
        *** TODO ***
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game = hEuclidean (game_hunter game) (position (head (game_targets game)))

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică.
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***
        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors game@(BonusGame g) = let
                        gameS = (South, BonusGame (advanceGameState South False g))
                        gameN = (North, BonusGame (advanceGameState North False g))
                        gameW = (West,  BonusGame (advanceGameState West  False g))
                        gameE = (East,  BonusGame (advanceGameState East  False g))
                        statesList = [gameW, gameE, gameS, gameN]
                    in
                        foldl (\acc x -> if snd x == game || x `elem` acc then acc else x : acc) [] statesList

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal (BonusGame g) =
        not(null (foldl (\acc x -> if isTargetKilled (game_hunter g) x then x : acc else acc) [] (game_targets g)))


    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h (BonusGame g) =
                    if minDistanceToAnyGate == 99999 then 
                        minWithNoGates
                    else
                        minDistanceToAnyGate
                    where
                        hunter = game_hunter g

                        minDistanceToAnyGate = foldl (\minVal x ->
                                                    min minVal (min (hEuclidean hunter (fst x)) (hEuclidean hunter (snd x)))) 99999 (game_gateways g)
                        minWithNoGates = foldl (\minVal x ->
                                            if minVal > hEuclidean hunter (position x) then
                                                                                hEuclidean hunter (position x)
                                                                            else minVal)
                                                                            999999 (game_targets g)