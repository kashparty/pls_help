module TTableGenerator where
import Parser
import Data.List
import Evaluator

type TTable = [([(Char, Bool)], ExtBool)]

unique :: (Eq a, Ord a) => [a] -> [a]
unique
  = map head . group . sort

getVars :: ASTNode -> [Char]
getVars (ASTVar c)
  = [c]
getVars (ASTNot node)
  = getVars node
getVars (ASTOr left right)
  = unique (leftVars ++ rightVars)
  where
    leftVars = getVars left
    rightVars = getVars right
getVars (ASTAnd left right)
  = unique (leftVars ++ rightVars)
  where
    leftVars = getVars left
    rightVars = getVars right
getVars (ASTImplies left right)
  = unique (leftVars ++ rightVars)
  where
    leftVars = getVars left
    rightVars = getVars right
getVars (ASTIff left right)
  = unique (leftVars ++ rightVars)
  where
    leftVars = getVars left
    rightVars = getVars right
getVars _
  = []

varAssignments :: [Char] -> [[(Char, Bool)]]
varAssignments []
  = [[]]
varAssignments (c : cs)
  = thisFalse ++ thisTrue
  where
    thisFalse = map ((c, False) :) $ varAssignments cs
    thisTrue = map ((c, True) :) $ varAssignments cs

generateTTable :: ASTNode -> TTable
generateTTable node
  = map (\ass -> (ass, eval ass node)) assignments 
  where
    vars = getVars node
    assignments = varAssignments vars