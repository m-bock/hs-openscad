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

-- Let's make this as LISPy as possible.

indent :: Int -> String
indent depth = replicate (depth * 2) ' '

renderAt :: Int -> Ast -> String
renderAt depth ast = indent depth ++ case ast of
  App name args [] ->
    name ++ renderArgs args ++ ";"
  App name args children ->
    "{" ++ name ++ renderArgs args ++ renderChildrenAt (depth + 1) children ++ "}"

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
renderArgs args = "(" ++ (intercalate "," $ map renderArg args) ++ ")"

renderArg :: (Maybe String, Lit) -> String
renderArg (name, arg) = case name of
  Just n  -> n ++ "=" ++ renderLit arg
  Nothing -> renderLit arg

renderChildrenAt :: Int -> [Ast] -> String
renderChildrenAt depth xs = intercalate "" $ map (addNl . renderAt depth) xs

addNl :: String -> String
addNl s = "\n" ++ s