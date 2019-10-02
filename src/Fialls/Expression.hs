module Fialls.Expression where

import Prelude hiding (LT, GT)
import Data.List (isInfixOf)
import Data.Function (on)

-- See https://www.elastic.co/guide/en/logstash/current/event-dependent-configuration.html
-- for expression documentation.
--
-- https://github.com/elastic/logstash/blob/master/logstash-core/src/main/java/org/logstash/config/ir/compiler/EventCondition.java

data Expression
  = Variable String
  | Literal Value
  | Not Expression
  | BinaryOp Expression Op Expression
  -- The match operators are not normal binary operators since they don't
  -- operate on an Expression on their right hand side.  The RHS is always
  -- a literal String representing the pattern to match.
  | Match Expression String
  | NotMatch Expression String
  deriving (Show)

data Op
  = And
  | Or
  | In
  | NotIn
  | Eq
  | NotEq
  | LT
  | LTE
  | GT
  | GTE
  deriving (Show)

type Environment = String -> Value

eval :: Expression -> Environment -> Value
eval expr env = case expr of
  Variable var        -> env var
  Literal val         -> val
  Not a               -> not' $ eval a env
  Match a pattern     -> eval a env `match` pattern
  NotMatch a pattern  -> not' $ eval a env `match` pattern
  BinaryOp a opName b -> eval a env `op` eval b env
    where op = applyOp opName

applyOp :: Op -> Value -> Value -> Value
applyOp opName a b = fromBool $ a `op` b
  where
    op = case opName of
      And   -> ((&&) `on` truthy)
      Or    -> ((||) `on` truthy)
      In    -> in'
      NotIn -> notIn'
      Eq    -> (==)
      NotEq -> (/=)
      LT    -> (<)
      LTE   -> (<=)
      GT    -> (>)
      GTE   -> (>=)

truthy :: Value -> Bool
truth val = case val of
  UndefinedValue -> False
  FalseValue     -> False
  _              -> True

in' :: Value -> Value -> Bool
x `in'` (ListValue xs)            = x `elem` xs
(StrValue s1) `in'` (StrValue s2) = s1 `isInfixOf` s2
_ `in'` _                         = False

notIn' :: Value -> Value -> Bool
x `notIn'` y = not $ x `in'` y

fromBool :: Bool -> Value
fromBool True  = TrueValue
fromBool False = FalseValue

not' :: Value -> Value
not' = fromBool . not . truthy

match :: Value -> String -> Value
match val pattern = case val of
  (StrValue s) -> fromBool $ s `matchString` pattern
  _            -> FalseValue

-- This should be a Ruby style match, as in:
-- https://ruby-doc.org/core-2.4.0/String.html#method-i-match
-- Text.Regex.PCRE might be a good fit.
matchString :: String -> String -> Bool
matchString s pattern = undefined