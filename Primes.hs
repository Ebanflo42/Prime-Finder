import Data.List
import Control.Monad

data Cmd = Bool | List deriving Eq

readInput :: String -> Either Cmd (Cmd, Int)
readInput "List"                         = Left List
readInput ('L':'i':'s':'t':restOfString) = Right (List, read restOfString :: Int)
readInput ('B':'o':'o':'l':restOfString) = Right (Bool, read restOfString :: Int)
readInput any                            = error "Couldn't read input"

primeList :: [Int]
primeList = [7, 5, 3, 2]

processList :: [Int] -> [Int] -> [Int]
processList list []     = list
processList list (x:xs) = if any (\n -> x `mod` n == 0) list then processList list xs
    else processList (x : list) xs

makeOutput :: [Int] -> Int -> ([Int], Bool)
makeOutput list n = let first = head list in if n < first then (list, not (all (n/=) list))
    else let result = processList list [first + 2..n] in (result, n == head result)

loop pList input = let cmd = readInput input in
    case cmd of
        Left List  -> putStrLn (show pList) >> (getLine >>= loop pList)
        Right cmd' -> let output = makeOutput pList (snd cmd'); list = fst output in
            if fst cmd' == List then
                putStrLn (show list) >> (getLine >>= loop list)
                else putStrLn (show (snd output)) >> (getLine >>= loop list)
        any        -> error "The impossible happened"

main = getLine >>= loop primeList
