module SimpleCalculator (calculator) where
import Parsing

data Op = Plus | Minus deriving Show
data Expr= Con Int | Bin Op Expr Expr deriving Show

expr :: Parser Expr
expr = token (constant <|> paren binary)
constant = do {n <- nat; return (Con n)}
binary = do { e1 <- expr; p <- op; e2 <- expr; return (Bin p e1 e2)}
op = (symbol "+" >> return Plus) <|> (symbol "-" >> return Minus)

exec :: Expr -> Int
exec (Con x) = x
exec (Bin Plus y z) = (exec y) + (exec z)
exec (Bin Minus y z) = (exec y) - (exec z)

calculator = do {
    x <- getLine;
    if x /= "quit"
    then do {
        putStrLn $ show $ fmap (exec . fst)  (apply expr x);
        calculator
    }
    else return ()
}