# pcubature

<!-- badges: start -->
[![Stack](https://github.com/stla/pcubature/actions/workflows/Stack-lts.yml/badge.svg)](https://github.com/stla/pcubature/actions/workflows/Stack-lts.yml)
[![Stack](https://github.com/stla/pcubature/actions/workflows/Stack-nightly.yml/badge.svg)](https://github.com/stla/pcubature/actions/workflows/Stack-nightly.yml)
<!-- badges: end -->

***Multiple integration over convex polytopes.***

***Warning:*** the package does not work in GHCi.

___

This package allows to evaluate a multiple integral over a convex polytope. 
Let's consider for example the following integral:

$$\int\_0^1\int\_0^1\int\_0^1 \exp(x+y+z)\\,\text{d}z\\,\text{d}y\\,\text{d}x = {(e-1)}^3 \approx 5.07321411177285.$$

The domain of integration is the cube ${[0,1]}^3$. In order to use the package, 
one has to provide the vertices of this cube:

```haskell
integrateOnPolytope'
    :: (Vector Double -> Double) -- ^ integrand
    -> [[Double]]                -- ^ vertices of the polytope
    -> Int                       -- ^ maximum number of evaluations
    -> Double                    -- ^ desired absolute error
    -> Double                    -- ^ desired relative error
    -> Int                       -- ^ integration rule: 1, 2, 3 or 4
    -> IO Result                 -- ^ value, error estimate, evaluations, success
```

Let's go:

```haskell
module Main 
  where
import Numeric.Integration.PolyhedralCubature
import Data.Vector.Unboxed as V

f :: Vector Double -> Double
f v = exp (V.sum v)

cube :: [[Double]]
cube = [
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
integral = integrateOnPolytope' f cube 10000 0 1e-6 3

main :: IO ()
main = do 
  i <- integral
  print i
-- Result {
--          value = 5.073214090351428
--        , errorEstimate = 2.8421152805879766e-6
--        , evaluations = 710
--        , success = True
--        }
```

This cube is axis-aligned. So it may be better to use the **adaptive-cubature** 
package here. The **pcubature** package allows to evaluate multiple integrals 
whose bounds are (roughly speaking) linear combinations of the variables, 
such as:

$$\int\_{-5}^4\int\_{-5}^{3-x}\int\_{-10}^{6-2x-y} f(x, y, z)\\,\text{d}z\\,\text{d}y\\,\text{d}x.$$

Here, the domain of integration is given by the set of linear inequalities:

$$\left\{\begin{matrix} -5  & \leq & x & \leq & 4 \\\ -5  & \leq & y & \leq & 3-x \\\ -10 & \leq & z & \leq & 6-2x-y \end{matrix}\right.$$

Each of these linear inequalities defines a halfspace of $\mathbb{R}^3$, and 
the intersection of these six halfspaces is a convex polytope (a polyhedron).

But it is not easy to get the vertices of this polytope. This is why the 
**pcubature** package depends on the **vertexenum** package, whose purpose is 
to enumerate the vertices of a polytope given as above, with linear 
inequalities. Let's take as example the function $f(x,y,z) = x(x+1) - yz^2$:

```haskell
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
integral = integrateOnPolytope' f polytope 10000 0 1e-6 3

main :: IO ()
main = do 
  i <- integral
  print i
-- Result {
--          value = 74321.77499999988
--        , errorEstimate = 1.0533262499999988e-7
--        , evaluations = 330
--        , success = True
--        }
```

The exact value of this integral is $74321.775$, as we shall see later.

The function $f$ of this example is polynomial. So we can use the function 
`integratePolynomialOnPolytope` to integrate it. This requires to define 
the polynomial with the help of the **hspray** package; we also import some 
modules of the **numeric-prelude** package, which allows to define a **hspray** 
polynomial more conveniently:

```haskell
module Main
  where
import Numeric.Integration.PolyhedralCubature
import Geometry.VertexEnum
import Data.VectorSpace     ( 
                              AdditiveGroup((^+^), (^-^))
                            , VectorSpace((*^)) 
                            )
import Math.Algebra.Hspray  ( Spray, lone, (^**^) )
import Prelude hiding       ( (*), (+), (-) )
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module                
import Algebra.Ring

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
```

The function `integratePolynomialOnPolytope` implements an exact procedure. 
However we didn't get the exact result. That's because of (small) 
numerical errors. The first numerical errors occur in the vertex enumeration 
performed by the **vertexenum** package:

```haskell
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
--   [-5.000000000000003, 8.000000000000004, 8.000000000000004]
-- , [-4.999999999999998, -4.999999999999996, 20.999999999999993]
-- , [3.999999999999999, -0.9999999999999997, -1.0]
-- , [3.999999999999999, -5.0, 3.0000000000000004]
-- , [-5.0, -5.0, -10.0]
-- , [-5.0, 8.000000000000002, -10.0]
-- , [4.0, -0.9999999999999999, -10.0]
-- , [4.0, -5.0, -10.0]
-- ]
```

Since all coefficients of the linear inequalities are rational (they even are 
integral), the vertices should be rational as well. 
Unfortunately, **vertexenum** only allows to get vertices with double 
coordinates. So if we want to use `Rational`, we have to manually enter 
the vertices:

```haskell
module Main
  where
import Numeric.Integration.PolyhedralCubature
import Math.Algebra.Hspray  ( Spray, lone, (^**^) )
import Prelude hiding       ( (*), (+), (-) )
import qualified Prelude as P
import Algebra.Additive              
import Algebra.Module                
import Algebra.Ring

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
-- 2972871 % 40
```

We get it, the exact value $74321.775$, as promised.