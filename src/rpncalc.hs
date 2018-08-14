import Data.List

solveRPN :: String -> Double
solveRPN = head . foldl rpn [] . words
    where   rpn (x:y:xs) "+" = (x + y):xs
            rpn (x:y:xs) "-" = (y - x):xs
            rpn (x:y:xs) "*" = (x * y):xs
            rpn (x:y:xs) "/" = (y / x):xs
            rpn (x:y:xs) "^" = (y ** x):xs
            rpn (x:xs) "log" = log x:xs
            rpn xs numString = read numString:xs

main = do
    input <- getLine
    print $ solveRPN input
