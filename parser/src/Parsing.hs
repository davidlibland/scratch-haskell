module Parsing where
import Prelude hiding (guard, fail)
import Data.Char
import Data.Maybe


-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"

newtype Parser a = Parser (String -> Maybe (a, String))

apply :: Parser a -> String -> Maybe (a, String)
apply (Parser p) s = p s

parse :: Parser a -> String -> a
parse p = fst . fromJust . apply p

(<**>) :: (a -> b) -> (c -> d) -> ((a, c) -> (b, d))
(<**>) f g (x, y) = (f x, g y)

instance Functor Parser where
    fmap f (Parser g) = Parser ((fmap (f <**> id)) . g)

instance Applicative Parser where
    pure x = Parser (\s -> Just (x, s))
    f <*> p = f >>= (\f -> fmap f p)

instance Monad Parser where
    p >>= q = Parser (\s -> do{
        (x, s') <- apply p s;
        apply (q x) s'
    })

getc :: Parser Char
getc = Parser f where
    f [] = Nothing
    f (c:cs) = Just (c, cs)

fail = Parser (\s -> Nothing)

guard :: Bool -> Parser ()
guard True = return ()
guard False = fail

sat :: (Char -> Bool) -> Parser Char
sat p = do {
    c <- getc;
    guard (p c);
    return c
}

char :: Char -> Parser ()
char x = do {c <- sat (==x); return ()}

string :: String -> Parser ()
string [] = return ()
string (x:xs) = do {char x; string xs; return ()}

lower :: Parser Char
lower = sat isLower

digit :: Parser Int
digit = do {d <- sat isDigit; return (cvt d)} where
    cvt d = fromEnum d - fromEnum '0'

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser (\s -> let ps = apply p s in
    if isNothing ps
    then apply q s
    else ps)

some :: Parser a -> Parser [a]
some p = do {x <- p; xs <- many p; return (x:xs)}

many :: Parser a -> Parser [a]
many p = optional (some p)

optional :: Parser [a] -> Parser [a]
optional p = p <|> none

none = return []

lowers :: Parser String
lowers = many lower

space :: Parser ()
space = many (sat isSpace) >> return ()

token :: Parser a -> Parser a
token p = space >> p

symbol :: String -> Parser ()
symbol s = token (string s)

natural = token nat
nat :: Parser Int
nat = fmap (foldl1 (\d m -> 10*d + m)) (some digit)

int :: Parser Int
int = space >> (minus <*> nat)

minus :: Parser (Int -> Int)
minus = (char '-' >> return negate) <|> return id

ints :: Parser [Int]
ints = brackets (manywith (symbol ",") int)

brackets :: Parser a -> Parser a
brackets p = do {symbol "["; x <- p; symbol "]"; return x}

paren :: Parser a -> Parser a
paren p = do {symbol "("; x <- p; symbol ")"; return x}

manywith :: Parser () -> Parser a -> Parser [a]
manywith q p = optional (somewith q p)

somewith :: Parser () -> Parser a -> Parser [a]
somewith q p = do {x <- p; xs <- many (q >> p); return
(x:xs)}

upto :: Char -> Parser String
upto c = Parser (\s ->
    let (xs, ys) = break (==c) s in
        if null ys
        then Nothing
        else Just (xs, tail ys))