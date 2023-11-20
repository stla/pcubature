module Main
  where
import Numeric.Integration.PolyhedralCubature
import Geometry.VertexEnum
import Data.VectorSpace     ( 
                              AdditiveGroup((^+^), (^-^))
                            , VectorSpace((*^)) 
                            )
import Prelude hiding       ( (*), (+), (-) )
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module                
import Algebra.Ring     
import Math.Algebra.Hspray  ( Spray, (^**^) )

p :: Spray Double
p v = x * (x + 1) - (y * z^**^2) 
  where
    x = lone 1 :: Spray Double
    y = lone 2 :: Spray Double
    z = lone 3 :: Spray Double

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
integral = integratePolynomialOnPolytope' p polytope

main :: IO ()
main = do 
  i <- integral
  print i
-- Result {value = 74321.77499999988, errorEstimate = 1.0533262499999988e-7, evaluations = 330, success = True}