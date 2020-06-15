module Ui where
import Calculator as Clc
import ParserFundamentals

runCalculator :: IO ()
runCalculator = do 
    inp <- getLine
    case inp of
        "q" -> print "Exiting calculator"
        _ -> do
            case (parse parseExpression inp) of
                (ParseSuccess expr "") -> print (calculate expr)
                (ParseSuccess expr str) -> print ("Parse error: unexpected " ++ str)
                (ParseError str) -> print ("Parse error: unexpected " ++ str)
            runCalculator
        

