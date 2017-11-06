--TODO: reverse all the elements in the prime list file and fix printing
import Data.List

data Output  = Bool | List
data Command a = Print Output | Calculate (Output, a) | Fail

--reads input if the user wants to use Integers
readInputLong :: String -> Command Integer
readInputLong "List"                         = Print List
readInputLong ('L':'i':'s':'t':restOfString) = Calculate (List, read restOfString :: Integer)
readInputLong ('B':'o':'o':'l':restOfString) = Calculate (Bool, read restOfString :: Integer)
readInputLong any                            = Fail

--reads input if the user wants to use Ints
readInputShort :: String -> Command Int
readInputShort "List"                         = Print List
readInputShort ('L':'i':'s':'t':restOfString) = Calculate (List, read restOfString :: Int)
readInputShort ('B':'o':'o':'l':restOfString) = Calculate (Bool, read restOfString :: Int)
readInputShort any                            = Fail

--int list to the entries separated by spaces and ending in a new line
toStringShort :: [Int] -> String
toStringShort []     = "\n"
toStringShort (x:xs) = "\n" ++ (show x) ++ (toStringShort xs)

--integer list to the entries separated by spaces and ending in a new line
toStringLong :: [Integer] -> String
toStringLong []     = "\n"
toStringLong (x:xs) = "\n" ++ (show x) ++ (toStringLong xs)

--default primes
pL = [7,5,3,2]
{-
findPrimes :: Integral a => [a] -> [a] -> [a]
findPrimes list []     = list
findPrimes list (x:xs) = 
    if any (\n -> x `mod` n == 0) list then findMultiples list xs
    else findMultiples (x : list) xs
-}
findPrimes list1 list2 = list1 ++ (filter (\x -> not (any (\n -> x `mod` n /= 0) list1)) list2)

determineResult :: Integral a => [a] -> a -> ([a], Bool)
determineResult list n = 
    let highestPrime = head list in 
        if n < highestPrime then (list, not (all (n/=) list))
        else let result = findPrimes list [highestPrime + 2..n] in (result, n == head result)

loopLong :: [Integer] -> String -> IO () 
loopLong pList input =
    case readInputLong input of
        Print List     -> putStrLn (toStringLong pList) >> (getLine >>= loopLong pList)
        Calculate pair -> let result = determineResult pList (snd pair); newList = fst result in
            case fst pair of
                List -> putStrLn (toStringLong newList) >> (getLine >>= loopLong newList)
                Bool -> putStrLn (show $ snd result) >> (getLine >>= loopLong newList)
        Fail           -> putStrLn "Unable to read input." >> (getLine >>= loopLong pList)

loopShort :: [Int] -> String -> IO () 
loopShort pList input =
    case readInputShort input of
        Print List     -> putStrLn (toStringShort pList) >> (getLine >>= loopShort pList)
        Calculate pair -> let result = determineResult pList (snd pair); newList = fst result in
            case fst pair of
                List -> putStrLn (toStringShort newList) >> (getLine >>= loopShort newList)
                Bool -> putStrLn (show $ snd result) >> (getLine >>= loopShort newList)
        Fail           -> putStrLn "Unable to read input." >> (getLine >>= loopShort pList)

requestListPreference :: IO ()
requestListPreference = do
    putStrLn "Load prime list? (y/n)"
    yesno <- getLine
    if yesno == "y" then do
        contents <- readFile "primeList.txt"
        let primeList = ((\s -> read s :: [Int]) . tail . (dropWhile (\c -> c /= '\n'))) contents
        putStrLn "Prime list loaded."
        getLine >>= loopShort primeList
    else if yesno == "n" then
        putStrLn "Using single digit primes." 
          >> getLine >>= loopShort pL
    else
        putStrLn "Please give valid input." 
          >> requestListPreference

main = do
    putStrLn "Would you like to use integers with unlimited storage? (y/n)"
    response <- getLine
    if response == "y" then do
        contents <- readFile "primeList.txt"
        let primeList = ((\s -> read s :: [Integer]) . tail . (dropWhile (\c -> c /= '\n'))) contents
        putStrLn "Prime list loaded."
        getLine >>= loopLong primeList
    else if response == "n" then requestListPreference
    else putStrLn "Please give valid input." >> main