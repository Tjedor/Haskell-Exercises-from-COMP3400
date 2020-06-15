{-# LANGUAGE RankNTypes #-}

module Sudoku where
import SudokuDataTypes



-- Main method, takes a Board and returns all solutions 
-- if no variables exist, emits the board
-- otherwise solves the board's next variable
sudoku :: Board Cell -> Logic (Board Digit)
sudoku bc = solver (generateConstraints bh) bh where bh = cellBoardToHoleBoard bc


-- Shows the first solution of a board
showFirst bc = case logicHead (sudoku bc) of
    Just sol -> print sol
    Nothing -> print "No solution"


-- Shows all solutions returned by sudoku method
showAll bc = print (iterateLog (uncons (sudoku bc)))
iterateLog (Just tup) =  (fst tup): iterateLog (uncons (snd tup))
iterateLog Nothing = []





-- list of all sudoku rules applied to a board
generateConstraints:: Board Hole -> [Constraint]
generateConstraints b = aggr (squares b) ++ aggr (rows b) ++ aggr (cols b)

squares :: Board a -> [Four a]
squares (Board (Four a b c d)) = [a, b, c, d]

rows :: Board a -> [Four a]
rows (Board (Four (Four a b c d) (Four e f g h) (Four i j k l) (Four m n o p))) = [(Four a b e f), (Four c d g h), (Four i j m n), (Four k l o p)]

cols :: Board a -> [Four a]
cols (Board (Four (Four a b c d) (Four e f g h) (Four i j k l) (Four m n o p))) = [(Four a c i k), (Four b d j l), (Four e g m o), (Four f h n p)]

aggr:: [Four Hole] -> [Constraint]
aggr [a, b, c, d] = unitConstraints a ++ unitConstraints b ++ unitConstraints c ++ unitConstraints d

unitConstraints:: Four Hole -> [Constraint]
unitConstraints (Four a b c d) = genUnitConstraints a [] ++ genUnitConstraints b [a] ++ genUnitConstraints c [a, b] ++ genUnitConstraints d [a, b, c]

genUnitConstraints:: Hole -> [Hole] -> [Constraint]
genUnitConstraints b [] = []
genUnitConstraints b (x:xs) = (NotEqual b x): (genUnitConstraints b xs )




-- creates variables for unknown cells
cellToHole:: Cell -> State Int Hole
cellToHole Unknown = do 
    s <- get
    put (s + 1)
    return (Variable s)

cellToHole (Known d) = do 
    return (Concrete d)
    
-- Takes a boar dof cells and returns a Board of holes where eavh unknown cell is given a variable with a unique int
cellBoardToHoleBoard :: Board Cell -> Board Hole
cellBoardToHoleBoard cb = evalState (traverse ((cellToHole)) cb) 0



-- replaces a variable in a board
substitute :: Int -> Digit -> Board Hole -> Board Hole
substitute id d b = fmap (sub id d) b

sub :: Int -> Digit -> Hole -> Hole
sub id d (Variable a) = if a == id then Concrete d else Variable a
sub id d (Concrete d2) = Concrete d2





-- replaces a variable in a constraint
instantiate :: Int -> Digit -> Constraint -> Constraint
instantiate id d (NotEqual h1 h2) = case (h1) of 
    (Variable a) ->  if a == id then NotEqual (Concrete d) h2 else
        case h2 of 
            (Variable b) -> if b == id then NotEqual h1 (Concrete d) else NotEqual h1 h2
            _ -> NotEqual h1 h2
    Concrete d2 -> case h2 of 
            (Variable b) -> if b == id then NotEqual h1 (Concrete d) else NotEqual h1 h2
            _ -> NotEqual h1 h2




-- Checks if any of the constraints are violated, returns true if none are violated.
checkConstraints:: [Constraint] -> Bool
checkConstraints ((NotEqual h1 h2):xs) = if h1 == h2 then False else checkConstraints xs
checkConstraints [] = True


-- if no variables exist, emits the board
-- otherwise solves the board's next variable
solver :: [Constraint] -> Board Hole ->  Logic (Board Digit)
solver c b = if onlyConc c then cons (fmap (holeToDig) b) nil else
    let
        (Just (Variable a)) = getVariable c
        m1 = try (Variable a) c D1
        m2 = try (Variable a) c D2
        m3 = try (Variable a) c D3
        m4 = try (Variable a) c D4
        in concatLogic (cons (runSolver m1 a D1 b) (cons (runSolver m2 a D2 b) (cons (runSolver m3 a D3 b) (cons (runSolver m4 a D4 b) nil))))


runSolver:: Maybe [Constraint] -> Int -> Digit -> Board Hole -> Logic (Board Digit)
runSolver (Just nc) a d bb = solver nc (substitute a d bb)
runSolver Nothing a d bb = nil


-- Instantiates a variable hole and checks if it is a valid selection
try:: Hole -> [Constraint] -> Digit -> Maybe [Constraint]
try (Variable a) c d = if checkConstraints nc then Just nc else Nothing where nc = fmap (instantiate a d) c


-- return the first variable hole it comes across, Nothing if all are concrete
getVariable :: [Constraint] -> Maybe Hole
getVariable (x:xs) = case extractVar x of
    Just (Variable a) -> Just (Variable a)
    _ -> getVariable xs
getVariable [] = Nothing

-- returns true if all constraints contain only Concrete holes
onlyConc :: [Constraint] -> Bool
onlyConc [] = True
onlyConc ((NotEqual h1 h2):xs) = case (h1, h2) of
    (Concrete a, Concrete b) -> onlyConc xs
    _ -> False


holeToDig :: Hole -> Digit
holeToDig (Concrete d) = d   
holeToDig (Variable a) = D4              




-- if either of the holes in a constraint are variables those are returned, else nothing
extractVar :: Constraint -> Maybe Hole
extractVar (NotEqual h1 h2) = case h2 of
    (Variable b) -> Just (Variable b)
    _ -> case h1 of
        (Variable c) -> Just (Variable c)
        _ -> Nothing

 
prettyFour :: (a -> Char) -> Four a -> Four a -> String
prettyFour s (Four a b c d) (Four e f g h) =
    unlines [ "/--\\/--\\", "|" ++ [s a, s b] ++ "||" ++ [s e, s f] ++ "|", "|" ++ [s c, s d] ++ "||" ++ [s g, s h] ++ "|", "\\--/\\--/"]
        
prettyBoard :: (a -> Char) -> Board a -> String
prettyBoard s (Board (Four a b c d)) =
    prettyFour s a b ++ prettyFour s c d



-- Board for testing
cf = Four (Known D4) (Unknown) (Known D2) (Unknown)
cf2 = Four (Known D2) (Unknown) (Known D4) (Unknown)
cf3 = Four (Unknown) (Known D4) (Unknown) (Known D2)
cf4 = Four (Unknown) (Known D2) (Unknown) (Known D4)
bf = Board (Four cf cf2 cf3 cf4)


