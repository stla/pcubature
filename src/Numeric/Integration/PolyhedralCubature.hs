module Numeric.Integration.PolyhedralCubature
	where

import Data.Vector.Unboxed                 ( Vector )
import Geometry.Delaunay                   ( delaunay )
import Geometry.VertexEnum                 ( Constraints(..)
	                                         , vertexenum 
	                                         )
import Math.Algebra.Hspray                 ( Spray(..) )
import Numeric.Integration.SimplexCubature ( Result(..)
	                                         , Results(..)
	                                         , integrateOnSimplex
	                                         , integrateOnSimplex'
	                                         )

type VectorD = Vector Double
