module ParserFundamentals where

data ParseResult x =
    ParseError String
    | ParseSuccess x String
    deriving (Eq, Show)

data Parser x =
    Parser (String -> ParseResult x)


-- returns a parser for chars that fulfill a specific condition
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser (
  \s -> case s of
    "" -> ParseError "end of input"
    (c:rest) | pred c -> ParseSuccess c rest
    a -> ParseError a
  )

-- Apply the method of a parser
parse:: Parser x -> String -> ParseResult x
parse (Parser f) str = f str

-- parser for digits
digit:: Parser Char
digit = satisfy isDigit

-- parser for a specific character
char:: Char -> Parser Char
char c = satisfy (== c)

-- parser for natural numbers
nat:: Parser Int
nat = do 
    n <- some digit
    return (read n::Int)

-- parser for ints
int:: Parser Int
int = do
        sign <- char '-'
        n <- nat
        return (-n)
       <|> nat


many :: Parser a -> Parser ([a])
many p = ((fmap (:)) p) <*> many p <|> pure []

some :: Parser a -> Parser [a]
some p = ((fmap (:)) p) <*> many p
 

isDigit:: Char -> Bool
isDigit x = case x of
    '1' -> True
    '2' -> True
    '3' -> True
    '4' -> True
    '5' -> True
    '6' -> True
    '7' -> True
    '8' -> True
    '9' -> True
    '0' -> True
    _ -> False

--Instances To gain some code reuse, add the following type-class instances:

instance Functor ParseResult where -- fmap :: (a -> b) -> ParseResult a -> ParseResult b
    fmap f (ParseSuccess a str) = ParseSuccess (f a) str
    fmap f (ParseError str) = ParseError str

instance Functor Parser where -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser (\str -> fmap f (parse p str))
    
instance Applicative Parser where
   pure v = Parser (\str -> ParseSuccess v str)

   f <*> px = Parser (\str -> case parse f str of  -- applicative :: Parser (a -> b) -> Parser a -> Parser b
                             ParseError x -> ParseError x
                             ParseSuccess x xs -> parse (fmap x px) xs)

instance Monad Parser where -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = Parser (\str -> case parse p str of  
                           ParseError x -> ParseError x
                           ParseSuccess x xs -> parse (f x) xs)

-- Or operator 
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser (\str -> case parse p1 str of
                            ParseError _ -> parse p2 str
                            ParseSuccess res rest -> ParseSuccess res rest)
infixl 3 <|>  -- NOTE, lower precendence than <$>, <*>, etc.
