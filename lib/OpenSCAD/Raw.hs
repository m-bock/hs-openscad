{- FOURMOLU_DISABLE -}

module OpenSCAD.Raw (Ast(..), Lit(..), render) where

import Data.List (intercalate)

-------------------------------------------------------------------------------
-- / Types
-------------------------------------------------------------------------------

type Comment = String

data Ast
  = App String [(Maybe String, Lit)] [Ast]
  | Comment Comment Ast

data Lit
  = LitVec [Lit]
  | LitDouble Double
  | LitInt Int
  | LitBool Bool

-------------------------------------------------------------------------------
-- / Render
-------------------------------------------------------------------------------

render :: Ast -> String
render = renderAt 0

renderAt :: Int -> Ast -> String
renderAt depth ast = case ast of
  (App name args children) ->
    indent depth ++ name ++ renderArgs args ++ renderChildrenAt depth children
  (Comment comment ast) ->
    indent depth ++ "// " ++ comment ++ renderAt depth ast

renderLit :: Lit -> String
renderLit = \case
  LitVec vs ->
    "[" ++ intercalate ", " (map renderLit vs) ++ "]"
  LitDouble x -> formatDouble x
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

-------------------------------------------------------------------------------
-- / Helpers
-------------------------------------------------------------------------------

isRoundIntEps :: Double -> Bool
isRoundIntEps x = abs (x - fromInteger (round x)) < 1e-9

formatDouble :: Double -> String
formatDouble x
  | isRoundIntEps x = show (round x :: Integer)
  | otherwise       = show x

indent :: Int -> String
indent depth = "\n" <> replicate (depth * 2) ' '
