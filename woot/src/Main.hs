module Main where
import System.Random

wordsFilename = "Words.txt"

getWords :: String -> IO [String]
getWords filename = do{
    wordString <- readFile filename;
    return (words wordString)
}

chooseWord :: [String] -> IO String
chooseWord wordSet = do{
    n <- randomIO :: IO Int;
    return (wordSet!!(n `mod` length wordSet))
}

contains :: Eq a => [a] -> a -> Bool
contains (x:xs) y = if x == y then True else contains xs y
contains _ y = False

replaceUnknown :: [Char] -> Char -> Char
replaceUnknown known x = if contains known x then x else '_'

playRound :: String -> [Char] -> IO ()
playRound word guessChars = do{
    putStrLn "I am thinking of a word:";
    putStrLn (map (replaceUnknown guessChars) word);
    putStrLn "Try and guess it.";
    testWord <- getLine;
    if testWord == word
    then return ()
    else playRound word (testWord ++ guessChars)
}

play :: [String] -> IO ()
play loadedWords = do{
    chosenWord <- chooseWord loadedWords;
    playRound chosenWord [];
    putStrLn "You won!";
    putStrLn "Would you like to play again? (yes/no):";
    answer <- getLine;
    if answer == "yes"
    then play loadedWords
    else return ()
}

main :: IO ()
main = do {
    loadedWords <- getWords wordsFilename;
    play loadedWords
}
