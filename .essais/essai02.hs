module Main
  where
import Numeric.Integration.PolyhedralCubature
import Geometry.VertexEnum
import Data.VectorSpace     ( 
                              AdditiveGroup((^+^), (^-^))
                            , VectorSpace((*^)) 
                            )
import Data.Vector.Unboxed  as V

f :: Vector Double -> Double
f v = x * (x+1) - y * z * z
  where
    x = v ! 0
    y = v ! 1
    z = v ! 2

polytope :: [Constraint Double]
polytope = [
             x .>= (-5)         -- shortcut for `x .>=. cst (-5)`
           , x .<=  4
           , y .>= (-5)
           , y .<=. cst 3 ^-^ x -- we need `cst` here
           , z .>= (-10)
           , z .<=. cst 6 ^-^ 2*^x ^-^ y 
           ]
           where
             x = newVar 1
             y = newVar 2
             z = newVar 3

integral :: IO Result
integral = integrateOnPolytope' f polytope 100000 0 1e-6 3

main :: IO ()
main = do 
  i <- integral
  print i
