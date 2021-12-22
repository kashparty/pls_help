import TTableGenerator
import Parser
import Evaluator
import Debug.Trace
import Data.List
import Simplifier (fromConjuncts, fromDisjuncts)

intToBin :: Int -> [Int]
intToBin n
  | n < 2     = [n]
  | otherwise = (n `mod` 2) : intToBin (n `div` 2)

binZerosAt :: Int -> Int -> [Int]
binZerosAt d n
  = [p | (d, p) <- zip extBinDigits (iterate (2*) 1), d == 0]
  where
    binDigits = intToBin n
    extBinDigits = binDigits ++ replicate (d - length binDigits) 0

getMerges :: [[Int]] -> Int -> [Int] -> [[Int]]
getMerges candidates d ms@(m : _)
  = [ms ++ c | c <- lookingFor, c `elem` candidates]
  where
    minAdd = last ms - m + 1
    toAdd = filter (>= minAdd) $ binZerosAt d m
    lookingFor = [map (a +) ms | a <- toAdd]

findPrimeImplicants :: [[Int]] -> Int -> [[Int]]
findPrimeImplicants candidates d
  | candidates == newCandidates = newCandidates
  | otherwise                   = findPrimeImplicants newCandidates d
  where
    merges = concatMap (getMerges candidates d) candidates
    newCandidates = merges ++ filter (any (`notElem` concat merges)) candidates

findEssentialPrimes :: [Int] -> [[Int]] -> [[Int]]
findEssentialPrimes minterms primes
  | null minterms' = singleOccs
  | otherwise      = p : singleOccs ++ findEssentialPrimes newMinterms ps
  where
    occs = map (\m -> filter (m `elem`) primes) minterms
    singleOccs = unique $ map (\[x] -> x) $ filter ((1 ==) . length) occs
    minterms' = filter (`notElem` concat singleOccs) minterms
    primes' = filter (`notElem` singleOccs) primes
    (p : ps) = sortOn (\p -> -(length $ filter (`elem` p) minterms')) primes'
    newMinterms = filter (`notElem` p) minterms' 

primeToNode :: TTable -> [Int] -> ASTNode
primeToNode ttable prime 
  = fromConjuncts nodes 
  where
    combinations = [m | ((m, _), i) <- zip ttable [0..], i `elem` prime]
    allConjuncts = unique $ concat combinations
    common = filter (\c -> all (c `elem`) combinations) allConjuncts
    nodes = map (\(c, v) -> if v then ASTVar c else ASTNot (ASTVar c)) common

minimise :: ASTNode -> ASTNode 
minimise node
  = fromDisjuncts disjuncts
  where
    ttable = generateTTable node
    minterms = [i | ((_, v), i) <- zip ttable [0..], v == ExtTrue]
    primeImplicants = findPrimeImplicants (map (: []) minterms) (length ttable)
    sortedPrimes = sortOn (\ms -> -(length ms)) primeImplicants
    essentialPrimes = findEssentialPrimes minterms sortedPrimes
    disjuncts = map (primeToNode ttable) essentialPrimes