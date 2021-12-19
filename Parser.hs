module Parser (
  ASTNode(
    ASTVar,
    ASTTop,
    ASTBottom,
    ASTNot,
    ASTOr,
    ASTAnd,
    ASTImplies,
    ASTIff
  ),
  parse
) where

data Token = Var Char 
           | Top
           | Bottom
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
  show Top
    = "T"
  show Bottom
    = "F"
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
      'T' -> Top      : getTokens xs
      'F' -> Bottom   : getTokens xs
      '(' -> LBracket : getTokens xs
      ')' -> RBracket : getTokens xs
      '~' -> Not      : getTokens xs
      '&' -> And      : getTokens xs
      '|' -> Or       : getTokens xs
      '-' -> Implies  : getTokens (tail xs)
      '<' -> Iff      : getTokens (tail $ tail xs)
      v   -> Var v    : getTokens xs

data ASTNode = ASTVar Char
             | ASTTop
             | ASTBottom
             | ASTNot ASTNode
             | ASTAnd ASTNode ASTNode
             | ASTOr ASTNode ASTNode
             | ASTImplies ASTNode ASTNode
             | ASTIff ASTNode ASTNode
             deriving (Eq)

instance Show ASTNode where
  show (ASTVar c)
    = [c]
  show ASTTop
    = "T"
  show ASTBottom
    = "F"
  show (ASTNot node)
    = "~" ++ show node
  show (ASTAnd left right)
    = "(" ++ show left ++ " & " ++ show right ++ ")"
  show (ASTOr left right)
    = "(" ++ show left ++ " | " ++ show right ++ ")"
  show (ASTImplies left right)
    = "(" ++ show left ++ " -> " ++ show right ++ ")"
  show (ASTIff left right)
    = "(" ++ show left ++ " <-> " ++ show right ++ ")"

parsePrimary :: [Token] -> (ASTNode, [Token])
parsePrimary (LBracket : ts)
  = (node, rest)
  where
    (node, RBracket : rest) = parseIff ts
parsePrimary (Top : ts)
  = (ASTTop, ts)
parsePrimary (Bottom : ts)
  = (ASTBottom, ts)
parsePrimary (Not : ts)
  = parseNot (Not : ts)
parsePrimary (Var c : ts)
  = (ASTVar c, ts)

parseNot :: [Token] -> (ASTNode, [Token])
parseNot (Not : ts)
  = (ASTNot node, rest)
  where
    (node, rest) = parsePrimary ts
parseNot ts
  = parsePrimary ts

parseAnd :: [Token] -> (ASTNode, [Token])
parseAnd ts
  = parsePartialAnd left rest 
  where
    (left, rest) = parseNot ts

    parsePartialAnd :: ASTNode -> [Token] -> (ASTNode, [Token])
    parsePartialAnd left (And : rest)
      = (ASTAnd left right, rest')
      where
        (right, rest') = parseAnd rest
    parsePartialAnd left rest
      = (left, rest)

parseOr :: [Token] -> (ASTNode, [Token])
parseOr ts
  = parsePartialOr left rest
  where
    (left, rest) = parseAnd ts

    parsePartialOr :: ASTNode -> [Token] -> (ASTNode, [Token])
    parsePartialOr left (Or : rest)
      = (ASTOr left right, rest')
      where
        (right, rest') = parseOr rest
    parsePartialOr left rest
      = (left, rest)

parseImplies :: [Token] -> (ASTNode, [Token])
parseImplies ts
  = parsePartialImplies left rest
  where
    (left, rest) = parseOr ts

    parsePartialImplies :: ASTNode -> [Token] -> (ASTNode, [Token])
    parsePartialImplies left (Implies : rest)
      = (ASTImplies left right, rest')
      where
        (right, rest') = parseImplies rest
    parsePartialImplies left rest
      = (left, rest)

parseIff :: [Token] -> (ASTNode, [Token])
parseIff ts
  = parsePartialIff left rest
  where
    (left, rest) = parseImplies ts

    parsePartialIff :: ASTNode -> [Token] -> (ASTNode, [Token])
    parsePartialIff left (Iff : rest)
      = (ASTIff left right, rest')
      where
        (right, rest') = parseIff rest
    parsePartialIff left rest
      = (left, rest)

parse :: String -> ASTNode
parse s
  | null rest  = node
  | otherwise  = error "Incomplete parse" 
  where
    (node, rest) = parseIff $ getTokens s
