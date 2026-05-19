{- FOURMOLU_DISABLE -}

module OpenSCAD.Raw (Ast(..), Lit(..), render) where

import Data.List (intercalate)

data Ast
  = App String [(Maybe String, Lit)] [Ast]

data Lit
  = LitVec [Lit]
  | LitDouble Double
  | LitInt Int
  | LitBool Bool

render :: Ast -> String
render = renderAt 0

indent :: Int -> String
indent 0 = ""
indent depth = "\n" <> replicate (depth * 2) ' '

renderAt :: Int -> Ast -> String
renderAt depth (App name args children) = 
  indent depth ++ name ++ renderArgs args ++ renderChildrenAt depth children

renderLit :: Lit -> String
renderLit = \case
  LitVec vs ->
    "[" ++ intercalate "," (map renderLit vs) ++ "]"
  LitDouble x ->
    show x
  LitInt x ->
    show x
  LitBool x -> if x then "true" else "false"

renderArgs :: [(Maybe String, Lit)] -> String
renderArgs args = "(" ++ (intercalate ", " $ map renderArg args) ++ ")"

renderArg :: (Maybe String, Lit) -> String
renderArg (name, arg) = case name of
  Just n  -> n ++ "=" ++ renderLit arg
  Nothing -> renderLit arg

renderChildrenAt :: Int -> [Ast] -> String
renderChildrenAt depth xs = case xs of
  [] -> ";"
  [x] -> renderAt (depth + 1) x
  xs -> "{" ++ (intercalate "" $ map (renderAt (depth + 1)) xs) ++ indent depth ++ "}"
