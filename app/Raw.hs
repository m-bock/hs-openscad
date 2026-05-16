{-# LANGUAGE LambdaCase #-}
module Raw where

import Data.List (intercalate)

data Ast = App String [Ast] [Ast]
         | Id String
         | Vec3 Ast Ast Ast
         | LitDouble Double

render :: Ast -> String
render = \case
  App name args1 args2 -> 
    "{" ++ name ++ "(" ++ renderArgsWith "," args1 ++ ")" ++ renderArgsWith "" args2 ++ ";}"
  Id name -> name
  Vec3 x y z -> "[" ++ render x ++ "," ++ render y ++ "," ++ render z ++ "]"
  LitDouble x -> show x

renderArgsWith :: String -> [Ast] -> String
renderArgsWith sep = intercalate sep . map render