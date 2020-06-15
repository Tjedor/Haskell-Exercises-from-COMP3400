module Ui where 
import Pick15API

-- Initializes a gamestate and starts the game
playPick15 :: IO ()
playPick15 = do
    handleInput "1 2 3 4 5 6 7 8 9" gs
    where gs = newGame

-- handles each turn after the first depending on the value of NumberSelected
continue:: NumberSelected -> [Char] -> GameState -> IO () 
continue (Selected (Player1Wins a) ) availalbe  previous = do
    putStrLn "Player 1 wins"
    print a

continue (Selected (Player2Wins a) ) available previous = do
    print "Player 2 wins"
    print a

continue (Selected Draw) available previous = putStrLn "The game is a draw"

continue (Selected (KeepPlaying (gs))) available previous = do
    handleInput available gs

continue (AlreadySelected num) available previous = do
    putStrLn "already selected"
    handleInput available previous
    
continue InvalidSelection available previous = do
    putStrLn "Invalid input"
    handleInput available previous

-- checks the user input for errors and goes to continue
handleInput:: [Char] -> GameState -> IO()
handleInput available currentState = do
    print (available)
    n <- getLine
    if (length n == 1 )then
        if (head n) == 'q' 
            then putStrLn "Thanks for playing" 
            else case inputToNumber_1to9 (head n) of
                Nothing -> continue InvalidSelection available currentState
                (Just m) -> continue (selectNumber (m) (currentState)) (filter (\x -> x /= (head n)) available) currentState
        else continue InvalidSelection available currentState
        

-- helper method to handle errors in input
inputToNumber_1to9:: Char -> Maybe Number_1to9
inputToNumber_1to9 n = case n of
    '1' -> Just N1
    '2' -> Just N2
    '3' -> Just N3 
    '4' -> Just N4
    '5' -> Just N5
    '6' -> Just N6
    '7' -> Just N7
    '8' -> Just N8
    '9' -> Just N9
    _ -> Nothing