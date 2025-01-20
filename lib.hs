{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SpacedRepetition where

import Data.List (genericIndex)
import Data.Fixed (mod')

-- Newtypes for strong typing
newtype R = R Double deriving (Show, Eq, Ord, Num, Fractional, Floating)
newtype S = S Double deriving (Show, Eq, Ord, Num, Fractional, Floating)
newtype D = D Double deriving (Show, Eq, Ord, Num, Fractional, Floating)
newtype T = T Double deriving (Show, Eq, Ord, Num, Fractional, Floating)

-- Weights array as a strongly typed list
weights :: [Double]
weights =
    [ 0.40255, 1.18385, 3.173, 15.69105, 7.1949, 0.5345, 1.4604, 0.0046,
      1.54575, 0.1192, 1.01925, 1.9395, 0.11, 0.29605, 2.2698, 0.2315,
      2.9898, 0.51655, 0.6621
    ]

-- Grade Enum
data Grade = Forgot | Hard | Good | Easy
    deriving (Show, Eq, Enum)

gradeToDouble :: Grade -> Double
gradeToDouble Forgot = 1.0
gradeToDouble Hard   = 2.0
gradeToDouble Good   = 3.0
gradeToDouble Easy   = 4.0

-- Constants
fConst, cConst :: Double
fConst = 19.0 / 81.0
cConst = -0.5

-- Function definitions
retrievability :: T -> S -> R
retrievability (T t) (S s) = R $ (1.0 + fConst * (t / s)) ** cConst

interval :: R -> S -> T
interval (R rD) (S s) = T $ (s / fConst) * ((rD ** (1.0 / cConst)) - 1.0)

s0 :: Grade -> S
s0 g = S $ weights !! fromEnum g

sSuccess :: D -> S -> R -> Grade -> S
sSuccess (D d) (S s) (R r) g =
    let tD = 11.0 - d
        tS = s ** (-weights !! 9)
        tR = exp (weights !! 10 * (1.0 - r)) - 1.0
        h  = if g == Hard then weights !! 15 else 1.0
        b  = if g == Easy then weights !! 16 else 1.0
        c  = exp (weights !! 8)
        alpha = 1.0 + tD * tS * tR * h * b * c
    in S $ s * alpha

sFail :: D -> S -> R -> S
sFail (D d) (S s) (R r) =
    let dF = d ** (-weights !! 12)
        sF = ((s + 1.0) ** weights !! 13) - 1.0
        rF = exp (weights !! 14 * (1.0 - r))
        cF = weights !! 11
        sF' = dF * sF * rF * cF
    in S $ min sF' s

stability :: D -> S -> R -> Grade -> S
stability d s r Forgot = sFail d s r
stability d s r g      = sSuccess d s r g

clampD :: D -> D
clampD (D d) = D $ max 1.0 (min 10.0 d)

d0 :: Grade -> D
d0 g =
    let gVal = gradeToDouble g
    in clampD $ D (weights !! 4 - exp (weights !! 5 * (gVal - 1.0)) + 1.0)

difficulty :: D -> Grade -> D
difficulty d g = clampD $ D (weights !! 7 * unD (d0 Easy) + (1.0 - weights !! 7) * dp d g)
  where
    unD (D x) = x

dp :: D -> Grade -> Double
dp (D d) g = d + deltaD g * ((10.0 - d) / 9.0)

deltaD :: Grade -> Double
deltaD g = -weights !! 6 * (gradeToDouble g - 3.0)
