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
vertices = vertexenum polytope Nothing

main :: IO ()
main = do 
  vs <- vertices
  print vs
-- [
--   [-5.000000000000003,8.000000000000004,8.000000000000004]
-- , [-4.999999999999998,-4.999999999999996,20.999999999999993]
-- , [3.999999999999999,-0.9999999999999997,-1.0]
-- , [3.999999999999999,-5.0,3.0000000000000004]
-- , [-5.0,-5.0,-10.0]
-- , [-5.0,8.000000000000002,-10.0]
-- , [4.0,-0.9999999999999999,-10.0]
-- , [4.0,-5.0,-10.0]
-- ]
