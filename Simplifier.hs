module Simplifier where
import Parser
import Evaluator

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
simplify (ASTOr ASTTop _)
  = ASTTop
simplify (ASTOr _ ASTTop)
  = ASTTop
simplify (ASTOr ASTBottom node)
  = simplify node
simplify (ASTOr node ASTBottom)
  = simplify node
simplify (ASTOr a r@(ASTAnd b c))
  | a' == b' || a' == c'       = a'
  | a' == r'                   = a'
  | a' == simplify (ASTNot r') = ASTTop
  | a == a' && r == r'         = ASTOr a' r'
  | otherwise                  = simplify $ ASTOr a' r'
  where
    a' = simplify a
    b' = simplify b
    c' = simplify c
    r' = simplify r
simplify (ASTOr l@(ASTAnd b c) a)
  = simplify (ASTOr a l)
simplify (ASTOr left right)
  | left' == right'                   = left'
  | left' == simplify (ASTNot right') = ASTTop
  | left == left' && right == right'  = ASTOr left right
  | otherwise                         = simplify $ ASTOr left' right'
  where
    left' = simplify left
    right' = simplify right

-- Simplify And
simplify (ASTAnd ASTBottom _)
  = ASTBottom
simplify (ASTAnd _ ASTBottom)
  = ASTBottom
simplify (ASTAnd ASTTop node)
  = node
simplify (ASTAnd node ASTTop)
  = node
simplify (ASTAnd a r@(ASTOr b c))
  | a' == b' || a' == c'       = a'
  | a' == r'                   = a'
  | a' == simplify (ASTNot r') = ASTBottom
  | a == a' && r == r'         = ASTAnd a r
  | otherwise                  = simplify $ ASTAnd a' r'
  where
    a' = simplify a
    b' = simplify b
    c' = simplify c
    r' = simplify r
simplify (ASTAnd l@(ASTOr b c) a)
  = simplify (ASTAnd a l)
simplify (ASTAnd left right)
  | left' == right'                   = left'
  | left' == simplify (ASTNot right') = ASTBottom
  | left == left' && right == right'  = ASTAnd left right
  | otherwise                         = simplify $ ASTAnd left' right'
  where
    left' = simplify left
    right' = simplify right

-- Simplify Implies
simplify (ASTImplies left right)
  = simplify $ ASTOr (ASTNot left) right

-- Simplify Iff
simplify (ASTIff left right)
  = simplify $ ASTAnd (ASTImplies left right) (ASTImplies right left)

simplify node
  = node