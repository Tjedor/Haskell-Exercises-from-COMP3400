module Calculator where
import Test.Framework as Tf
import ParserFundamentals

data Operation =
    Add
    | Subtract
    | Divide
    | Multiply
    deriving (Eq, Show)

data Expression =
    Number Int
    | Op Expression Operation Expression
    | Parens Expression
    deriving (Eq, Show)

-- parser for Add Operation
add = do 
    o <- char '+'
    return Add 

-- parser for Subtract Operation
sub = do 
    o <- char '-'
    return Subtract  

-- parser for Multiply Operation
mul = do 
    o <- char '*'
    return Multiply

-- parser for Divide Operation
divide = do 
    o <- char '/'
    return Divide 

-- Parser for any of the four operations (Add Subtract Divide Multiply)
operation:: Parser Operation
operation = do 
           add <|> sub <|> mul <|> divide

operationAddOrSubtract:: Parser Operation
operationAddOrSubtract = do add <|> sub 

operationMultiplyOrDivide:: Parser Operation
operationMultiplyOrDivide = do mul <|> divide


-- Parsers for the expression grammar term

--term: factor | term '*' factor | term '/' factor
term = do 
    x <- factor
    putLeft x
    where putLeft e =
            do
                char ' '
                o <- operationMultiplyOrDivide
                char ' '
                y <- factor
                case o of
                    Multiply -> (putLeft (Op e Multiply y))
                    Divide -> (putLeft (Op e Divide y))
            <|> return e


--factor : parenthesis expression | Int
factor = do
    p1 <- char '('
    n <- expr
    p2 <- char ')'
    return (Parens n)
    <|> do 
        n <- int
        return (Number n)

-- expression:: term | expression '+' term | expression '-' term
expr = do 
    x <- term
    putLeft x
    where putLeft e = do
            char ' '
            o <- operationAddOrSubtract
            char ' '
            a2 <- term
            case o of
                Add -> putLeft (Op e Add a2)
                Subtract -> putLeft (Op e Subtract a2)
            <|> return e


parseExpression = expr

-- Calulates the value of an expression
calculate :: Expression -> Int
calculate (Op exp1 o exp2) = calc (calculate exp1) o (calculate exp2)
calculate (Parens exp) = calculate exp
calculate (Number n) = n

-- performs calculations according to the Operation type
calc:: Int -> Operation -> Int -> Int
calc a o b = case o of 
    Add -> (a + b)
    Subtract -> (a - b)
    Multiply -> (a * b)
    Divide -> (quot a b)

-- removes simple expression like (5 * 1) or (5 + 0)
simplify:: Expression -> Expression
simplify (Op expr o expr2) = case ((simplify expr2), o) of
    (Number 0, Multiply) -> Number 0
    (Number 0, Add) -> simplify expr
    (Number 0, Subtract) -> simplify expr
    (Number 1, Multiply) -> simplify expr
    (Number 1, Divide) -> simplify expr
    _ -> case ((simplify expr, o)) of
        (Number 0, Multiply) -> Number 0
        (Number 0, Divide) -> Number 0
        (Number 0, Add) -> simplify expr2
        (Number 1, Multiply) -> simplify expr2
        _ -> (Op (simplify expr) o (simplify expr2))    

simplify (Parens expr) = Parens (simplify expr)
simplify (Number a) = Number a


-- Test stuff 
instance Arbitrary Expression where
    arbitrary = genExpression --fmap Number (choose (-100, 100)) --sized $ \size -> (choose (Number 1, Number 5)) 

-- Gen Expression for random expressions
genExpression :: Gen Expression
genExpression = frequency [(2, genNumberExpression), (1, genOpExpression), (1,  genParensExpression)]

-- Gen Operation for random Operations
genOp :: Gen Operation 
genOp = fmap pickOp (choose (1, 4))

-- maps from int to Operation
pickOp:: Int -> Operation
pickOp s = case s of
    1 -> Add
    2 -> Subtract
    3 -> Multiply
    4 -> Divide

-- Gen Expression for expressions starting with Op 
genOpExpression:: Gen Expression
genOpExpression = ((fmap (Op) genExpression) <*> genOp) <*> genExpression

-- Gen Expression for Number expressions
genNumberExpression :: Gen Expression
genNumberExpression = fmap Number (choose (-100, 100))

-- Gen Expression for expressions starting with Parens
genParensExpression:: Gen Expression
genParensExpression = fmap Parens genExpression

-- Runs all tests of simplify
runTests = do
    print("running: testAddRightIdentity")
    quickCheck testAddRightIdentity
    print("running: testAddLeftIdentity")
    quickCheck testAddLeftIdentity
    print("running: testSubtractRightIdentity")
    quickCheck testSubtractRightIdentity
    print("running: testMultiplyLeftZero")
    quickCheck testMultiplyLeftZero
    print("running: testMultiplyRightIdentity")
    quickCheck testMultiplyRightIdentity
    print("running: testMultiplyRightZero")
    quickCheck testMultiplyRightZero
    print("running: testMultiplyLeftIdentity")
    quickCheck testMultiplyLeftIdentity
    print("running: testDivideLeftZero")
    quickCheck testDivideLeftZero
    print("running: testDivideRightIdentity")
    quickCheck testDivideRightIdentity
    

-- ADD
testAddRightIdentity :: Expression -> Bool
testAddRightIdentity e = 
    simplify (Op e Add (Number 0)) == simplify e
testAddLeftIdentity :: Expression -> Bool
testAddLeftIdentity e = 
    simplify (Op (Number 0) Add e) == simplify e

--Sub
testSubtractRightIdentity :: Expression -> Bool
testSubtractRightIdentity expr =
    simplify (Op expr Subtract (Number 0)) == simplify expr

-- Multiply
testMultiplyLeftZero :: Expression -> Bool
testMultiplyLeftZero expr =
    simplify (Op (Number 0) Multiply expr) == Number 0
testMultiplyRightIdentity :: Expression -> Bool
testMultiplyRightIdentity expr =
    simplify (Op expr Multiply (Number 1)) == simplify expr
testMultiplyRightZero :: Expression -> Bool
testMultiplyRightZero expr =
    simplify (Op expr Multiply (Number 0)) == Number 0
testMultiplyLeftIdentity :: Expression -> Bool
testMultiplyLeftIdentity expr =
    simplify (Op (Number 1) Multiply expr) == simplify expr

--Divide
testDivideLeftZero :: Expression -> Bool
testDivideLeftZero expr =
    simplify (Op (Number 0) Divide expr) == Number 0
testDivideRightIdentity :: Expression -> Bool
testDivideRightIdentity expr =
    simplify (Op expr Divide (Number 1)) == simplify expr
