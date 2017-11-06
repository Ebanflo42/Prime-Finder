--TODO: reverse all the elements in the prime list file and fix printing
import Data.List

data Output  = Bool | List
data Command a = Print Output | Calculate (Output, a) | Fail | Quit

--reads input if the user wants to use Integers
readInputLong :: String -> Command Integer
readInputLong "List"                         = Print List
readInputLong ('L':'i':'s':'t':restOfString) = Calculate (List, read restOfString :: Integer)
readInputLong ('B':'o':'o':'l':restOfString) = Calculate (Bool, read restOfString :: Integer)
readInputLong "q"                            = Quit
readInputLong any                            = Fail

--reads input if the user wants to use Ints
readInputShort :: String -> Command Int
readInputShort "List"                         = Print List
readInputShort ('L':'i':'s':'t':restOfString) = Calculate (List, read restOfString :: Int)
readInputShort ('B':'o':'o':'l':restOfString) = Calculate (Bool, read restOfString :: Int)
readInputShort "q"                            = Quit
readInputShort any                            = Fail

--int list to the entries separated by spaces and ending in a new line
toStringShort :: Int -> Int -> [Int] -> String
toStringShort index len list =
    case list of
        []     -> "\n"
        (x:xs) -> (toStringShort (index + 1) len xs) ++ ' ':(show x) ++ (if index `mod` len == 0 then "\n" else "")

--integer list to the entries separated by spaces and ending in a new line
toStringLong :: Int -> Int -> [Integer] -> String
toStringLong index len list =
    case list of
        []     -> "\n"
        (x:xs) -> (toStringLong (index + 1) len xs) ++ ' ':(show x) ++ (if index `mod` len == 0 then "\n" else "")

--default primes
pL = [7,5,3,2]

--first arg is the list of pre-existing primes
--second arg is a list of numbers to be checked for factorability
findPrimes :: Integral a => [a] -> [a] -> [a]
findPrimes list []     = list
findPrimes list (x:xs) = 
    if any (\n -> x `mod` n == 0) list then findPrimes list xs
    else findPrimes (x : list) xs

--given list of primes and a number
--if the number is less than the head of the list returns whether or not the number is in the list
--else calculates all primes up to the number and returns the new list and whether of not the number is part of the listS
determineResult :: Integral a => [a] -> a -> ([a], Bool)
determineResult list n = 
    let highestPrime = head list in 
        if n < highestPrime then (list, not (all (n/=) list))
        else let result = findPrimes list [highestPrime + 2..n] in (result, n == head result)

--loop for user using unlimited storage ints
loopLong :: [Integer] -> String -> IO () 
loopLong pList input =
    case readInputLong input of
        Print List     -> putStrLn (toStringLong 0 10 pList) >> (getLine >>= loopLong pList)
        Calculate pair -> let result = determineResult pList (snd pair); newList = fst result in
            case fst pair of
                List -> putStrLn (toStringLong 0 10 newList) >> (getLine >>= loopLong newList)
                Bool -> putStrLn (show $ snd result) >> (getLine >>= loopLong newList)
        Fail           -> putStrLn "Unable to read input." >> (getLine >>= loopLong pList)
        Quit           -> return ()

--loop for user using limited storage ints
loopShort :: [Int] -> String -> IO () 
loopShort pList input =
    case readInputShort input of
        Print List     -> putStrLn (toStringShort 0 10 pList) >> (getLine >>= loopShort pList)
        Calculate pair -> let result = determineResult pList (snd pair); newList = fst result in
            case fst pair of
                List -> putStrLn (toStringShort 0 10 newList) >> (getLine >>= loopShort newList)
                Bool -> putStrLn (show $ snd result) >> (getLine >>= loopShort newList)
        Fail           -> putStrLn "Unable to read input." >> (getLine >>= loopShort pList)
        Quit           -> return ()

--ask the user if they would like to read the prime list file
requestListPreference :: IO ()
requestListPreference = do
    putStrLn "Load prime list? (y/n)"
    response <- getLine
    if response == "y" then do
        contents <- readFile "primeList.txt"
        let primeList = ((\s -> read s :: [Int]) . tail . (dropWhile (\c -> c /= '\n'))) contents
        putStrLn "Prime list loaded."
        getLine >>= loopShort primeList
    else if response == "n" then
        putStrLn "Using single digit primes." 
          >> getLine >>= loopShort pL
    else if response == "q" then return ()
    else
        putStrLn "Please give valid input." 
          >> requestListPreference

main = do
    putStrLn "q to quit"
    putStrLn "Would you like to use integers with unlimited storage? (y/n)"
    response <- getLine
    if response == "y" then do
        contents <- readFile "primeList.txt"
        let primeList = ((\s -> read s :: [Integer]) . tail . (dropWhile (\c -> c /= '\n'))) contents
        putStrLn "Prime list loaded."
        getLine >>= loopLong primeList
    else if response == "n" then requestListPreference
    else if response == "q" then return ()
    else putStrLn "Please give valid input." >> main