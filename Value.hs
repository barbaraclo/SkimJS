module Value (Value (..)) where

import Language.ECMAScript3.Syntax
data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Nil

    |Break
    |Return Value
    |Continue
    |List [Value]
    |Function Id [Id] [Statement]
--
-- Pretty Printer
--

instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
  show Nil = "undefined"

  show Break = "break"
  show Continue = "continue"
  show (List a) = show a
  show (Function (Id nome) listarg liststtm) = "Function" ++ nome ++ "("++ show listarg ++ ")" ++ show liststtm
  show (Return valor) = show valor
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)

--instance Eq Value where
--   (Int a) == (Int b) = a == b
--   (String b) == (String b) = a == b
  