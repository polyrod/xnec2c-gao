module OptFunc where

import Debug.Trace
import Types

optFunc :: OptimizingMode -> DirectiveMode -> (Fitness -> Float)
optFunc VSWR dm = \(Fitness s _ f) -> crookedSwr s $ swr s + fbr dm f
optFunc GAIN dm = \(Fitness _ g f) -> gain g + fbr dm f
optFunc VSWRGAIN dm = \(Fitness s g f) -> abs $ sqrt $ (swr s) ^ (5 :: Int) + (gain g) ^ (2 :: Int) + (fbr dm f) ^ (3 :: Int)

swrMaxThrs :: Float
swrMaxThrs = 1.5

crookedSwr :: Float -> Float -> Float
crookedSwr s f =
  if s > 100 || s < 0
    then 0
    else f

swr :: Float -> Float
swr s =
  let v = 100 / s
   in crookedSwr s v

gain :: Float -> Float
gain g = g

fbr :: DirectiveMode -> Float -> Float
fbr SYMMETRICAL 0 = 1000
fbr SYMMETRICAL f
  | f < 0.3 = 100 / 0.3 + (f * f)
  | otherwise = 10 / f
fbr DIRECTIVE f = f * f
