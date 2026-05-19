{- FOURMOLU_DISABLE -}

module OpenSCAD.Raw (Ast(..), render) where

import Data.List (intercalate)

data Ast = App String [(Maybe String, Ast)] [Ast]
         | Id String
         | Vec [Ast]
         | LitDouble Double
         | LitInt Int
         | LitBool Bool

render :: Ast -> String
render = \case
  App name args1 args2 ->
    "{" ++ name ++ "(" ++ renderArgs1 args1 ++ ")" ++ renderMany args2 ++ "}"

  Id name ->
    name

  Vec vs ->
    "[" ++ intercalate "," (map render vs) ++ "]"

  LitDouble x ->
    show x

  LitInt x ->
    show x

  LitBool x -> if x then "true" else "false"

renderArgs1 :: [(Maybe String, Ast)] -> String
renderArgs1 args = intercalate "," $ map (\(name, arg) -> case name of
    Just name -> name ++ "=" ++ render arg
    Nothing -> render arg
  ) args

renderMany :: [Ast] -> String
renderMany [] = ";"
renderMany xs = intercalate "" $ map render xs