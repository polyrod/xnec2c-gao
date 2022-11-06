module OptFunc where

import Types

optFunc :: OptimizingMode -> DirectiveMode -> (Fitness -> Float)
optFunc VSWR dm = \(Fitness s _ f) -> swr s + fbr dm f
optFunc GAIN dm = \(Fitness _ g f) -> gain g + fbr dm f
optFunc VSWRGAIN dm = \(Fitness s g f) -> swr s + gain g + fbr dm f

swr :: Float -> Float
swr s = 100 / s

gain :: Float -> Float
gain g = 30 * g

fbr :: DirectiveMode -> Float -> Float
fbr SYMMETRICAL 0 = 1000
fbr SYMMETRICAL f
  | f < 0.3 = 100 / 0.3 + (f * f)
  | otherwise = 10 / f
fbr DIRECTIVE f = 10 * f
