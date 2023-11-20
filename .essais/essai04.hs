module Main
  where
import Geometry.VertexEnum
import Data.VectorSpace     ( 
                              AdditiveGroup((^+^), (^-^))
                            , VectorSpace((*^)) 
                            )

polytope :: [Constraint Double]
polytope = [
             x .>= (-5)         
           , x .<=  4
           , y .>= (-5)
           , y .<=. cst 3 ^-^ x 
           , z .>= (-10)
           , z .<=. cst 6 ^-^ 2*^x ^-^ y 
           ]
           where
             x = newVar 1
             y = newVar 2
             z = newVar 3

vertices :: IO [[Double]]
vertices = vertexenum polytope

main :: IO ()
main = do 
  v <- vertices
  print v
-- 