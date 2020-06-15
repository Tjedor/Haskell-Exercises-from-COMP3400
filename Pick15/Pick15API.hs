module Pick15API where
import Prelude as P
-- a data type that denotes the numbers that each player may select from
data Number_1to9 = N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 deriving (Eq, Show)

-- I chose to stick with this list data-type as I thought I would learn more from writing my own code than use the core library
data List a = Nil | Cons a (List a) deriving (Eq, Show)

-- starts off empty and records each move as a player selects a number
data GameState = GameState (List Number_1to9) deriving (Eq, Show)

data Solution =
    Solution
    Number_1to9
    Number_1to9
    Number_1to9 deriving (Eq)

instance Show Solution where
    show (Solution x1 x2 x3) = show [selectionToInt x1, selectionToInt x2, selectionToInt x3] 

-- the possible results of a valid number selection
data ValidSelection a =
    Player1Wins Solution
    | Player2Wins Solution
    | Draw
    | KeepPlaying a deriving (Eq, Show)

-- the possible results of any number selection
data NumberSelected =
    InvalidSelection
    | AlreadySelected Number_1to9
    | Selected (ValidSelection GameState) deriving (Eq, Show)

-- start a new game
newGame :: GameState
newGame = GameState (Nil)

-- select a number from the game state
--todo invalid number (handled in Ui instead)
selectNumber :: Number_1to9 -> GameState -> NumberSelected
selectNumber num (GameState list) =
    if ( list `contains` num)
        then AlreadySelected num
        else Selected (getNewStatus (GameState (list `concatenate` (Cons num Nil))))

-- returns whether or not the game state is a draw
isDraw :: GameState -> Bool
isDraw (GameState list) = if listLength list == 9 then True else False






-- *********************************
--                                 *
-- Added methods                   *
--                                 *
-- *********************************

-- Checks the new gamestate and returns the new status (win/draw/keepPlaying)
getNewStatus:: GameState -> ValidSelection GameState
getNewStatus (GameState list) = newStatus where
    p1 =  sumsTo15 (getPlayer1 list 1 Nil )
    p2 = sumsTo15 (getPlayer2 list 1 Nil)
    newStatus = if p1 /= Nil 
        then Player1Wins (createSolution p1) 
        else if p2 /= Nil
            then Player2Wins (createSolution p2)
            else if isDraw (GameState list) 
                then Draw 
                else KeepPlaying (GameState list)
    
-- creates A Solution from alist of three Number_1to9
createSolution:: List Number_1to9 -> Solution 
createSolution (Cons x (Cons x2 (Cons x3 Nil))) = Solution x x2 x3


-- Checks if a list of three Number_1to9 sums to 15
sumsTo15 :: List (List Number_1to9 ) -> List Number_1to9
sumsTo15 Nil = Nil
sumsTo15 (Cons (Cons x (Cons x2 (Cons x3 Nil))) xs) = if ( selectionToInt x + selectionToInt x2 + selectionToInt x3 == 15) then (Cons x (Cons x2 (Cons x3 Nil))) else sumsTo15 xs


-- converts from Number1to9 to Integer N1 -> 1, N2 -> 2 etc.
selectionToInt :: Number_1to9 -> Integer
selectionToInt n = case n of
    N1 -> 1
    N2 -> 2 
    N3 -> 3 
    N4 -> 4
    N5 -> 5
    N6 -> 6
    N7 -> 7
    N8 -> 8
    N9 -> 9


-- This methods returns every number at odd indice of the gameState as these are player 1s selections
getPlayer1:: List Number_1to9  -> Integer -> List Number_1to9 -> List (List Number_1to9)
getPlayer1 Nil _ p1 = listPermutations p1 3
getPlayer1 (Cons x xs) n p1 = if n `mod` 2 == 1 then getPlayer1 xs (n + 1) (p1 `concatenate` Cons x Nil) else getPlayer1 xs (n + 1) p1


--This method returns every number at the even indice of the gameState as these are player 2s selections
getPlayer2:: List Number_1to9  -> Integer -> List Number_1to9 -> List (List Number_1to9)
getPlayer2 Nil _ p2 = listPermutations p2 3
getPlayer2 (Cons x xs) n p2 = if n `mod` 2 == 0 then getPlayer2 xs (n + 1) (p2 `concatenate` Cons x Nil) else getPlayer2 xs (n + 1) p2





-- *********************************
--                                 *
-- LIST HELPERS                    *
--                                 *
-- *********************************


-- returns length of the list
listLength :: List a -> Integer
listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs


instance P.Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (P.fmap f as)


-- returns all permutations of size N from a list
listPermutations :: List Number_1to9 -> Integer -> List (List Number_1to9)
listPermutations _ 0      = Cons Nil Nil
listPermutations Nil _     = Nil
listPermutations (Cons x xs) n = P.fmap (Cons x ) (listPermutations xs (n - 1)) `concatenate` (listPermutations xs n)


-- concatenate two lists
concatenate :: List a -> List a -> List a
concatenate Nil Nil = Nil
concatenate Nil (Cons x xs) = Cons x xs
concatenate (Cons x xs) (Cons x2 xs2) = Cons x (concatenate xs (Cons x2 xs2))
concatenate (Cons x xs) Nil = Cons x xs


class Contains f where
  contains :: Eq a => f a -> a -> Bool


-- instance for checking if a list contains a certain target
instance Contains List where
  contains Nil a = False
  contains (Cons x as) a = if x == a then True else contains as a 