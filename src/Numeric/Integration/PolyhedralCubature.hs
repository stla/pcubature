module Numeric.Integration.PolyhedralCubature
  ( integrateOnPolytope
  , integrateOnPolytope'
  , Result(..)
  )
  where
import qualified Data.IntMap.Strict        as IM
import Data.Vector.Unboxed                 ( Vector )
import Geometry.Delaunay                   ( delaunay
                                           , getDelaunayTiles 
                                           )
import Geometry.VertexEnum                 ( Constraint(..)
                                           , vertexenum 
                                           )
import Math.Algebra.Hspray                 ( Spray )
import Numeric.Integration.SimplexCubature ( Result(..)
                                           , Results(..)
                                           , integrateOnSimplex
                                           , integrateOnSimplex'
                                           )

type VectorD = Vector Double

integrateOnPolytope
    :: (VectorD -> VectorD)   -- ^ integrand
    -> [[Double]]             -- ^ vertices of the polytope
    -> Int                    -- ^ number of components of the integrand
    -> Int                    -- ^ maximum number of evaluations
    -> Double                 -- ^ desired absolute error
    -> Double                 -- ^ desired relative error
    -> Int                    -- ^ integration rule: 1, 2, 3 or 4
    -> IO Results             -- ^ values, error estimate, evaluations, success
integrateOnPolytope f vertices dim maxevals abserr relerr rule = do 
  tessellation <- delaunay vertices False False Nothing
  let simplices = map IM.elems (getDelaunayTiles tessellation)
  integrateOnSimplex f simplices dim maxevals abserr relerr rule

integrateOnPolytope'
    :: (VectorD -> Double)    -- ^ integrand
    -> [[Double]]             -- ^ vertices of the polytope
    -> Int                    -- ^ maximum number of evaluations
    -> Double                 -- ^ desired absolute error
    -> Double                 -- ^ desired relative error
    -> Int                    -- ^ integration rule: 1, 2, 3 or 4
    -> IO Result              -- ^ values, error estimate, evaluations, success
integrateOnPolytope' f vertices maxevals abserr relerr rule = do 
  tessellation <- delaunay vertices False False Nothing
  let simplices = map IM.elems (getDelaunayTiles tessellation)
  integrateOnSimplex' f simplices maxevals abserr relerr rule