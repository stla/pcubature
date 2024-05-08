module Main ( main ) where
import Data.Ratio           ( (%) )
import Numeric.Integration.PolyhedralCubature ( integratePolynomialOnPolytope )
import Math.Algebra.Hspray  ( Spray, lone, (^**^), (^*^), (^+^), (^-^), unitSpray )
import Test.Tasty           ( defaultMain, testGroup )
import Test.Tasty.HUnit     ( testCase, assertEqual, assertBool )


main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ 

    testCase "exact integral of a polynomial" $ do
      let
        x = lone 1 :: Spray Rational
        y = lone 2 :: Spray Rational
        z = lone 3 :: Spray Rational
        p = x ^*^ (x ^+^ unitSpray) ^-^ (y ^*^ z^**^2) 
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
      integral <- integratePolynomialOnPolytope p polytope
      assertEqual "" integral (2972871 % 40)

  ]
