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
import Math.Algebra.Hspray  ( Spray, lone, (^**^) )

p :: Spray Double
p = x * (x + one) - (y * z^**^2) 
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

integral :: IO Double
integral = integratePolynomialOnPolytope' p polytope

main :: IO ()
main = do 
  i <- integral
  print i
-- 74321.77499999967