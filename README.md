# pcubature

<!-- badges: start -->
[![Stack](https://github.com/stla/pcubature/actions/workflows/Stack.yml/badge.svg)](https://github.com/stla/pcubature/actions/workflows/Stack.yml)
<!-- badges: end -->

*Multiple integration on convex polytopes.*

___

$$\int\_0^1\int\_0^1\int\_0^1 \exp(x+y+z)\\,\text{d}z\\,\text{d}y\\,\text{d}x = {(e-1)}^3 \approx 5.07321411177285.$$

The domain of integration is the cube ${[0,1]}^3$. In order to use the package, 
one has to provide the vertices of this cube.

```haskell
import Numeric.Integration.PolyhedralCubature
import Data.Vector.Unboxed as V

f :: Vector Double -> Double
f v = exp (V.sum v)

cube :: [[Double]]
cube = [
       , [0, 0, 0]
       , [0, 0, 1]
       , [0, 1, 0]
       , [0, 1, 1]
       , [1, 0, 0]
       , [1, 0, 1]
       , [1, 1, 0]
       , [1, 1, 1]
       ]
```

