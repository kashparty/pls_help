data Token = Var Char 
           | LBracket 
           | RBracket 
           | Not 
           | And 
           | Or 
           | Implies 
           | Iff
           deriving (Show, Eq)

-- Syntax example: ~p -> ~((~q | r) & s)
getTokens :: String -> [Token]
getTokens []
  = []
getTokens (x : xs)
  = case x of
      ' ' -> getTokens xs
      '(' -> LBracket : getTokens xs
      ')' -> RBracket : getTokens xs
      '~' -> Not      : getTokens xs
      '&' -> And      : getTokens (tail xs)
      '|' -> Or       : getTokens (tail xs)
      '-' -> Implies  : getTokens (tail xs)
      '<' -> Iff      : getTokens (tail $ tail xs)
      v   -> Var v    : getTokens xs