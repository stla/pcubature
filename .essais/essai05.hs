module Main
  where
import Numeric.Integration.PolyhedralCubature
import Prelude hiding       ( (*), (+), (-) )
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module                
import Algebra.Ring     
import Math.Algebra.Hspray  ( Spray, lone, (^**^) )

p :: Spray Rational
p = x * (x + one) - (y * z^**^2) 
  where
    x = lone 1 :: Spray Rational
    y = lone 2 :: Spray Rational
    z = lone 3 :: Spray Rational

polytope :: [[Rational]]
polytope = [
           ]

integral :: IO Double
integral = integratePolynomialOnPolytope p polytope

main :: IO ()
main = do 
  i <- integral
  print i
-- 