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
             [-5, 8, 8]
           , [-5, -5, 21]
           , [4, -1, -1]
           , [4, -5, 3]
           , [-5, -5, -10]
           , [-5, 8, -10]
           , [4, -1, -10]
           , [4, -5, -10]
           ]

integral :: IO Rational
integral = integratePolynomialOnPolytope p polytope

main :: IO ()
main = do 
  i <- integral
  print i
-- 