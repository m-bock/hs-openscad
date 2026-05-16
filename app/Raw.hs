{-# LANGUAGE LambdaCase #-}
module Raw where

import Data.List (intercalate)

data Ast = App String [(Maybe String, Ast)] [Ast]
         | Id String
         | Vec [Ast]
         | LitDouble Double

render :: Ast -> String
render = \case
  App name args1 args2 -> 
    "{" ++ name ++ "(" ++ renderArgs1 args1 ++ ")" ++ renderArgs2 args2 ++ ";}"
  Id name -> name
  Vec vs -> "[" ++ intercalate "," (map render vs) ++ "]"
  LitDouble x -> show x

renderArgs1 :: [(Maybe String, Ast)] -> String
renderArgs1 args = intercalate "," $ map (\(name, arg) -> case name of
    Just name -> name ++ "=" ++ render arg
    Nothing -> render arg
  ) args

renderArgs2 :: [Ast] -> String
renderArgs2 args = intercalate "" $ map render args