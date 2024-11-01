module OptFunc where

import Types

optFunc :: OptimizingMode -> DirectiveMode -> (Fitness -> Float)
optFunc VSWR dm = \(Fitness s _ f) -> crookedSwr s $ swr s + fbr dm f
optFunc GAIN dm = \(Fitness _ g f) -> gain g + fbr dm f
optFunc VSWRGAIN dm = \(Fitness s g f) -> crookedSwr s $ swr s + gain g + fbr dm f

swrMaxThrs :: Float
swrMaxThrs = 1.5

crookedSwr :: Float -> Float -> Float
crookedSwr s f =
  if s > 100 || s < 0
    then 0
    else f

swr :: Float -> Float
swr s
  | s < swrMaxThrs = (swrMaxThrs - s) * 100 + (100 / s)
  | otherwise = 100 / s

gain :: Float -> Float
gain g = 30 * g

fbr :: DirectiveMode -> Float -> Float
fbr SYMMETRICAL 0 = 1000
fbr SYMMETRICAL f
  | f < 0.3 = 100 / 0.3 + (f * f)
  | otherwise = 10 / f
fbr DIRECTIVE f = f * f
