data Token = Var Char 
           | LBracket 
           | RBracket 
           | Not 
           | And 
           | Or 
           | Implies 
           | Iff
           deriving (Eq)

instance Show Token where
  show (Var c)
    = [c]
  show LBracket
    = "("
  show RBracket
    = ")"
  show Not
    = "~"
  show And
    = " & "
  show Or
    = " | "
  show Implies
    = " -> "
  show Iff
    = " <-> "
  showList []
    = (++ [])
  showList ts 
    = (concatMap show ts ++)

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
      '&' -> And      : getTokens xs
      '|' -> Or       : getTokens xs
      '-' -> Implies  : getTokens (tail xs)
      '<' -> Iff      : getTokens (tail $ tail xs)
      v   -> Var v    : getTokens xs