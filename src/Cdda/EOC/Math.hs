{-# LANGUAGE OverloadedStrings #-}
module Cdda.EOC.Math
  ( parenExpr
  , mathAssign
  , (=:)
  , mathAssignAdd
  , (+=)
  , mathAssignSubtract
  , (-=)
  , mathAssignTimes
  , (*=)
  , mathAssignDiv
  , (/=)
  , mathIncrement
  , (++)
  , mathDecrement
  , (.--)
  , mathAdd
  , (+)
  , mathSubtract
  , (-)
  , mathTimes
  , (*)
  , mathDiv
  , (/)
  , mathEqual
  , (==)
  , mathLess
  , (<)
  , mathLessEq
  , (<=)
  , mathGreater
  , (>)
  , mathGreaterEq
  , (>=)
  , mathFunc1
  , mathFunc2
  , mathFunc3
  ) where

import Prelude hiding ( (+), (-), (*), (/), (<), (>), (<=), (>=), (/=), (++), (==) )
import Data.Text
import qualified Data.Text as T
import Define.EOC

parenExpr :: MathExpr -> MathExpr
parenExpr (MathExpr expr) = MathExpr $ "(" <> expr <> ")"

_mathAssign :: Expr e => Val -> Text -> e -> Math
_mathAssign v op e = Math3 (toExpr v) (MathOp op) $ toExpr e

mathAssign :: Expr e => Val -> e -> Math
mathAssign v = _mathAssign v "="

(=:) :: Expr e => Val -> e -> Math
(=:) = mathAssign

mathAssignAdd :: Expr e => Val -> e -> Math
mathAssignAdd v = _mathAssign v "+="

(+=) :: Expr e => Val -> e -> Math
(+=) = mathAssignAdd

mathAssignSubtract :: Expr e => Val -> e -> Math
mathAssignSubtract v = _mathAssign v "-="

(-=) :: Expr e => Val -> e -> Math
(-=) = mathAssignSubtract

mathAssignTimes :: Expr e => Val -> e -> Math
mathAssignTimes v = _mathAssign v "*="

(*=) :: Expr e => Val -> e -> Math
(*=) = mathAssignTimes

mathAssignDiv :: Expr e => Val -> e -> Math
mathAssignDiv v = _mathAssign v "/="

(/=) :: Expr e => Val -> e -> Math
(/=) = mathAssignDiv

mathUnary :: Text -> Val -> Math
mathUnary op v = Math2 (toExpr v) (MathOp op)

mathIncrement :: Val -> Math
mathIncrement = mathUnary "++"

(++) :: Val -> Math
(++) = mathIncrement

mathDecrement :: Val -> Math
mathDecrement = mathUnary "--"

(.--) :: Val -> Math
(.--) = mathIncrement

mathOp :: (Expr e1, Expr e2) => Text -> e1 -> e2 -> MathExpr
mathOp op e1 e2 =
  let (MathExpr e1') = toExpr e1
      (MathExpr e2') = toExpr e2
   in MathExpr $ e1' <> op <> e2'

mathAdd :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
mathAdd = mathOp " + "

(+) :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
(+) = mathAdd

mathSubtract :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
mathSubtract = mathOp " - "

(-) :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
(-) = mathSubtract

mathTimes :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
mathTimes = mathOp " * "

(*) :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
(*) = mathTimes

mathDiv :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
mathDiv = mathOp " / "

(/) :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
(/) = mathDiv

mathEqual :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
mathEqual = mathOp " == "

(==) :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
(==) = mathEqual

mathLess :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
mathLess = mathOp " < "

(<) :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
(<) = mathLess

mathLessEq :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
mathLessEq = mathOp " <= "

(<=) :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
(<=) = mathLessEq

mathGreater :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
mathGreater = mathOp " > "

(>) :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
(>) = mathGreater

mathGreaterEq :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
mathGreaterEq = mathOp " >= "

(>=) :: (Expr e1, Expr e2) => e1 -> e2 -> MathExpr
(>=) = mathGreaterEq

infix 8 =:
infix 8 +=
infix 8 -=
infix 8 *=
infix 8 /=
infix 9 ++
infix 9 .--
infixr 9 +
infixr 9 -
infixr 9 *
infixr 9 /
infixr 9 <
infixr 9 <=
infixr 9 >
infixr 9 >=

mathFunc1 :: Expr e => Text -> e -> MathExpr
mathFunc1 f e = case toExpr e of
                  MathExpr e' -> MathExpr $ T.concat [f, "(", e', ")" ]

mathFunc2 :: (Expr e1, Expr e2) => Text -> e1 -> e2 -> MathExpr
mathFunc2 f e1 e2 = case (toExpr e1, toExpr e2) of
                      (MathExpr e1', MathExpr e2') -> MathExpr $ T.concat [f, "(", e1', ", ", e2', ")"]

mathFunc3 :: (Expr e1, Expr e2, Expr e3) => Text -> e1 -> e2 -> e3 -> MathExpr
mathFunc3 f e1 e2 e3 = case (toExpr e1, toExpr e2, toExpr e3) of
                      (MathExpr e1', MathExpr e2', MathExpr e3') ->
                        MathExpr $ T.concat [f, "(", e1', ", ", e2', ", " <> e3' <> ")"]
