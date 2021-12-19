module Evaluator (
  eval,
  ExtBool (
    ExtTrue,
    ExtFalse,
    ExtUnknown
  )
) where

import Parser 

data ExtBool = ExtTrue | ExtFalse | ExtUnknown
             deriving (Eq)

instance Show ExtBool where
  show ExtTrue
    = "True"
  show ExtFalse
    = "False"
  show ExtUnknown
    = "Unknown"

extNot :: ExtBool -> ExtBool
extNot ExtTrue
  = ExtFalse
extNot ExtFalse
  = ExtTrue
extNot ExtUnknown
  = ExtUnknown

extAnd :: ExtBool -> ExtBool -> ExtBool
extAnd ExtTrue ExtTrue
  = ExtTrue
extAnd ExtFalse _
  = ExtFalse
extAnd _ ExtFalse
  = ExtFalse
extAnd _ _
  = ExtUnknown

extOr :: ExtBool -> ExtBool -> ExtBool
extOr ExtFalse ExtFalse
  = ExtFalse
extOr ExtTrue _
  = ExtTrue
extOr _ ExtTrue
  = ExtTrue
extOr _ _
  = ExtUnknown

extEq :: ExtBool -> ExtBool -> ExtBool
extEq ExtUnknown _
  = ExtUnknown
extEq _ ExtUnknown
  = ExtUnknown
extEq ExtTrue ExtTrue
  = ExtTrue
extEq ExtFalse ExtFalse
  = ExtTrue
extEq _ _
  = ExtFalse

fromMaybeBool :: Maybe Bool -> ExtBool
fromMaybeBool (Just True)
  = ExtTrue
fromMaybeBool (Just False)
  = ExtFalse
fromMaybeBool Nothing
  = ExtUnknown

eval :: [(Char, Bool)] -> ASTNode -> ExtBool
eval env (ASTVar c)
  = fromMaybeBool $ lookup c env
eval env ASTTop
  = ExtTrue
eval env ASTBottom
  = ExtFalse
eval env (ASTNot node)
  = extNot $ eval env node
eval env (ASTAnd left right)
  = extAnd (eval env left) (eval env right)
eval env (ASTOr left right)
  = extOr (eval env left) (eval env right)
eval env (ASTImplies left right)
  = extOr (extNot (eval env left)) (eval env right)
eval env (ASTIff left right)
  = extEq (eval env left) (eval env right)