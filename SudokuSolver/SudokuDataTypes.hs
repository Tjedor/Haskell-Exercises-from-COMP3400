{-# LANGUAGE RankNTypes #-}

module SudokuDataTypes where
data Logic a = Logic (forall r. (a -> r -> r) -> r -> r)

--
-- Funcitons related to the logic data type
--

runLogic :: Logic a -> (a -> r -> r) -> r -> r
runLogic (Logic f) a r = f a r

cons :: a -> Logic a -> Logic a
cons a (Logic list) = Logic $ \cs nil -> cs a (list cs nil)

nil :: Logic a
nil = Logic $ \cons nil -> nil

logicHead :: Logic a -> Maybe a
logicHead list = runLogic list (const . Just) Nothing

uncons :: Logic a -> Maybe (a, Logic a)
uncons (Logic list) = list skk Nothing
    where skk a rest = Just (a, maybe nil (uncurry cons) rest)

concatLogic :: Logic (Logic a) -> Logic a
concatLogic doubleL = Logic $ \cons empty -> runLogic doubleL (\cl r -> runLogic cl cons r) empty



data State s a = State (s -> (a, s))

--
-- Funcitons related to the state data type
--
runState :: State s a -> s -> (a, s)
runState (State f) i = f i

get:: State s s
get = State $ \s -> (s,s)

put:: s -> State s ()
put s = State $ \_ -> ((), s)

execState:: State s a -> s -> s 
execState st s = snd (runState st s)

evalState:: State s a -> s -> a
evalState st i  = fst (runState st i)


data Digit
    = D1
    | D2    
    | D3
    | D4
    deriving (Eq, Ord, Show)

data Cell
    = Unknown
    | Known Digit
    deriving (Eq, Ord, Show)

data Four a
    = Four a a a a
    deriving (Eq, Ord, Show)

data Board a
    = Board (Four (Four a))
    deriving (Eq, Ord, Show)

data Hole
    = Concrete Digit
    | Variable Int
    deriving (Eq, Ord, Show)

data Constraint
    = NotEqual Hole Hole    
    deriving (Eq, Ord, Show)



instance Functor Logic where
    fmap f (Logic l) = Logic $ \c n -> l (c . f) n

instance Applicative Logic where
    pure a = Logic $ \c n -> c a n
    (<*>) (Logic fs) (Logic as) = Logic $ \c n -> fs (\f r -> as (c . f) r) n 

instance Monad Logic where
    return a = Logic $ \sk fk -> sk a fk
    (>>=) (Logic as) f = Logic $ \c n -> as (\a r -> runLogic (f a) c r) n 



first:: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor (State s) where
    -- (a -> b) -> State s a -> State s b
    fmap f st = State (\s -> first f (runState st s))

instance Applicative (State s) where
    --pure:: a -> State s a
    pure a = State (\s -> (a, s))
    --State s (a -> b) -> State s a -> State s b
    (<*>) sf sa = State $ \s -> spread (runState sf s) (runState sa) where spread (f, ns) rs = first f (rs ns)

instance Monad (State s) where
    -- a -> State s a
    return a = State (\s -> (a, s))
    -- State s a -> (a -> State s b) -> State s b
    (>>=) sa f = State $ \st -> spread (runState sa st) f where spread (a, s') f = runState (f a) s'



instance Functor Four where
    --(a -> b) -> Four a -> Four b
    fmap f (Four a b c d) = Four (f a) (f b) (f c) (f d)

instance Foldable Four where
    foldr fn p2 (Four a b c d) = fn a (fn b (fn c (fn d p2)) )

instance Traversable Four where
    --(a -> f b) -> Four a -> f (Four b)
    traverse fn (Four a b c d) = Four <$> fn a <*> fn b <*> fn c <*> fn d

atob (Concrete _ ) = Just 'c'
atob (Variable _) = Just 'n'

instance Functor Board where
    fmap f (Board (Four a b c d)) = Board (Four (fmap f a) (fmap f b) (fmap f c) (fmap f d))

instance Foldable Board where
    foldr fn p2 (Board (Four a b c d)) = foldr fn (foldr fn (foldr fn (foldr fn p2 d) c ) b) a 

instance Traversable Board where
    -- (a -> f b) -> Board a -> f (Board b)
    traverse fn (Board (Four a b c d)) = Board <$> (Four <$> (traverse fn a) <*> traverse fn b <*> traverse fn c <*> traverse fn d)
