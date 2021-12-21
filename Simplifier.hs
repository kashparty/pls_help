module Simplifier where
import Parser
import Evaluator
import Data.List
import Debug.Trace

unique :: (Eq a, Ord a) => [a] -> [a]
unique
  = map head . group . sort

disjuncts :: ASTNode -> [ASTNode]
disjuncts (ASTOr left right)
  = disjuncts left ++ disjuncts right
disjuncts node
  = [node]

fromDisjuncts :: [ASTNode] -> ASTNode
fromDisjuncts []
  = ASTBottom
fromDisjuncts [n]
  = n
fromDisjuncts ns 
  = foldr1 ASTOr ns

conjuncts :: ASTNode -> [ASTNode]
conjuncts (ASTAnd left right)
  = conjuncts left ++ conjuncts right
conjuncts node
  = [node]

fromConjuncts :: [ASTNode] -> ASTNode
fromConjuncts []
  = ASTTop
fromConjuncts [n]
  = n
fromConjuncts ns
  = foldr1 ASTAnd ns

simplify :: ASTNode -> ASTNode
-- Simplify Not
simplify (ASTNot ASTTop)
  = ASTBottom
simplify (ASTNot ASTBottom)
  = ASTTop
simplify (ASTNot (ASTNot node))
  = simplify node
simplify (ASTNot (ASTOr left right))
  = simplify $ ASTAnd (ASTNot left) (ASTNot right)
simplify (ASTNot (ASTAnd left right))
  = simplify $ ASTOr (ASTNot left) (ASTNot right)
simplify (ASTNot node)
  | node == node' = ASTNot node
  | otherwise     = simplify $ ASTNot node'
  where
    node' = simplify node

-- Simplify Or
simplify node@(ASTOr _ _)
  | isTop = ASTTop
  | otherwise = foldr1 ASTOr antiAbsorb
  where
    uniques = (unique . concatMap (disjuncts . simplify) . disjuncts) node
    notBot = filter (/= ASTBottom) uniques
    absorb = filter (\n -> all (`notElem` conjuncts n) $ filter (/= n) notBot) notBot
    antiAbsorb = [fromConjuncts $ filter (\m -> simplify (ASTNot m) `notElem` absorb) $ conjuncts n | n <- absorb]
    isTop = any (\n -> n == ASTTop || simplify (ASTNot n) `elem` antiAbsorb) antiAbsorb

simplify node@(ASTAnd _ _)
  | isBot = ASTBottom
  | otherwise = foldr1 ASTAnd antiAbsorb
  where
    uniques = (unique . concatMap (conjuncts . simplify) . conjuncts) node
    notTop = filter (/= ASTTop) uniques
    absorb = filter (\n -> all (`notElem` disjuncts n) $ filter (/= n) notTop) notTop
    antiAbsorb = [fromDisjuncts $ filter (\m -> simplify (ASTNot m) `notElem` absorb) $ disjuncts n | n <- absorb]
    isBot = any (\n -> n == ASTBottom || simplify (ASTNot n) `elem` antiAbsorb) antiAbsorb

-- Simplify Implies
simplify (ASTImplies left right)
  = simplify $ ASTOr (ASTNot left) right

-- Simplify Iff
simplify (ASTIff left right)
  = simplify $ ASTAnd (ASTImplies left right) (ASTImplies right left)

simplify node
  = node