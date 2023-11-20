module Main
  where
import Numeric.Integration.PolyhedralCubature
import Data.Vector.Unboxed as V

f :: Vector Double -> Double
f v = exp (V.sum v)

-- (exp(1) - 1)**3

polytope :: [[Double]]
polytope = [
             [0, 0, 0]
           , [0, 0, 1]
           , [0, 1, 0]
           , [0, 1, 1]
           , [1, 0, 0]
           , [1, 0, 1]
           , [1, 1, 0]
           , [1, 1, 1]
           ]

integral :: IO Result
integral = integrateOnPolytope' f polytope 100000 0 1e-6 3

main :: IO ()
main = do 
  i <- integral
  print i
