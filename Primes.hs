
data Output  = Bool | List
data Command = Print Output | Calculate (Output, Int) | Fail

readInput :: String -> Command
readInput "List"                         = Print List
readInput ('L':'i':'s':'t':restOfString) = Calculate (List, read restOfString :: Int)
readInput ('B':'o':'o':'l':restOfString) = Calculate (Bool, read restOfString :: Int)
readInput any                            = Fail

primeList :: [Int]
primeList = [7, 5, 3, 2]

findFactors :: [Int] -> [Int] -> [Int]
findFactors list []     = list
findFactors list (x:xs) = 
    if any (\n -> x `mod` n == 0) list then findFactors list xs
    else findFactors (x : list) xs

findPrimes :: [Int] -> Int -> ([Int], Bool)
findPrimes list n = 
    let highestPrime = head list in 
        if n < highestPrime then (list, not (all (n/=) list))
        else let result = findFactors list [highestPrime + 2..n] in (result, n == head result)

loop pList input =
    case readInput input of
        Print List     -> putStrLn (show pList) >> (getLine >>= loop pList)
        Calculate pair -> let result = findPrimes pList (snd pair); newList = fst result in
            case fst pair of
                List -> putStrLn (show newList) >> (getLine >>= loop newList)
                Bool -> putStrLn (show (snd result)) >> (getLine >>= loop newList)
        Fail           -> putStrLn "Unable to read input" >> (getLine >>= loop pList)

main = getLine >>= loop primeList
