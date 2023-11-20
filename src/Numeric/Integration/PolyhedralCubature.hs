module Numeric.Integration.PolyhedralCubature
  ( integrateOnPolytopeN
  , integrateOnPolytope
  , integrateOnPolytopeN'
  , integrateOnPolytope'
  , Result(..)
  , Results(..)
  , Constraint(..)
  , integratePolynomialOnPolytope
  , integratePolynomialOnPolytope'
  )
  where
import Algebra.Ring                                     ( C )
import qualified Data.IntMap.Strict                     as IM
import Data.Vector.Unboxed                              ( Vector )
import Geometry.Delaunay                                ( delaunay
                                                        , getDelaunayTiles 
                                                        )
import Geometry.VertexEnum                              ( Constraint(..)
                                                        , vertexenum 
                                                        )
import Math.Algebra.Hspray                              ( Spray )
import Numeric.Integration.IntegratePolynomialOnSimplex ( integratePolynomialOnSimplex )
import Numeric.Integration.SimplexCubature              ( Result(..)
                                                        , Results(..)
                                                        , integrateOnSimplex
                                                        , integrateOnSimplex'
                                                        )

type VectorD = Vector Double

integrateOnPolytopeN
    :: (VectorD -> VectorD)   -- ^ integrand
    -> [[Double]]             -- ^ vertices of the polytope
    -> Int                    -- ^ number of components of the integrand
    -> Int                    -- ^ maximum number of evaluations
    -> Double                 -- ^ desired absolute error
    -> Double                 -- ^ desired relative error
    -> Int                    -- ^ integration rule: 1, 2, 3 or 4
    -> IO Results             -- ^ values, error estimate, evaluations, success
integrateOnPolytopeN f vertices dim maxevals abserr relerr rule = do 
  tessellation <- delaunay vertices False False Nothing
  let simplices = map IM.elems (getDelaunayTiles tessellation)
  integrateOnSimplex f simplices dim maxevals abserr relerr rule

integrateOnPolytope
    :: (VectorD -> Double)    -- ^ integrand
    -> [[Double]]             -- ^ vertices of the polytope
    -> Int                    -- ^ maximum number of evaluations
    -> Double                 -- ^ desired absolute error
    -> Double                 -- ^ desired relative error
    -> Int                    -- ^ integration rule: 1, 2, 3 or 4
    -> IO Result              -- ^ values, error estimate, evaluations, success
integrateOnPolytope f vertices maxevals abserr relerr rule = do 
  tessellation <- delaunay vertices True False Nothing
  let simplices = map IM.elems (getDelaunayTiles tessellation)
  integrateOnSimplex' f simplices maxevals abserr relerr rule

integrateOnPolytopeN'
    :: Real a
    => (VectorD -> VectorD)   -- ^ integrand
    -> [Constraint a]         -- ^ linear inequalities defining the polytope
    -> Int                    -- ^ number of components of the integrand
    -> Int                    -- ^ maximum number of evaluations
    -> Double                 -- ^ desired absolute error
    -> Double                 -- ^ desired relative error
    -> Int                    -- ^ integration rule: 1, 2, 3 or 4
    -> IO Results             -- ^ values, error estimate, evaluations, success
integrateOnPolytopeN' f constraints dim maxevals abserr relerr rule = do 
  vertices <- vertexenum constraints Nothing
  integrateOnPolytopeN f vertices dim maxevals abserr relerr rule

integrateOnPolytope'
    :: Real a
    => (VectorD -> Double)    -- ^ integrand
    -> [Constraint a]         -- ^ linear inequalities defining the polytope
    -> Int                    -- ^ maximum number of evaluations
    -> Double                 -- ^ desired absolute error
    -> Double                 -- ^ desired relative error
    -> Int                    -- ^ integration rule: 1, 2, 3 or 4
    -> IO Result              -- ^ values, error estimate, evaluations, success
integrateOnPolytope' f constraints maxevals abserr relerr rule = do 
  vertices <- vertexenum constraints Nothing
  integrateOnPolytope f vertices maxevals abserr relerr rule

delaunay' :: Real a => [[a]] -> IO [[[a]]]
delaunay' points = do
  let points' = map (map realToFrac) points
  tessellation <- delaunay points' True False Nothing
  let indices = map IM.keys (getDelaunayTiles tessellation)
  return $ map (map (points !!)) indices

integratePolynomialOnPolytope
  :: (RealFrac a, C a)
  => Spray a -- ^ polynomial to be integrated
  -> [[a]]   -- ^ vertices of the polytope to integrate over
  -> IO a
integratePolynomialOnPolytope p vertices = do
  simplices <- delaunay' vertices
  let integrals = map (integratePolynomialOnSimplex p) simplices 
  return $ sum integrals

integratePolynomialOnPolytope'
  :: Spray Double        -- ^ polynomial to be integrated
  -> [Constraint Double] -- ^ vertices of the polytope to integrate over
  -> IO Double
integratePolynomialOnPolytope' p constraints = do
  vertices <- vertexenum constraints Nothing
  tessellation <- delaunay vertices True False Nothing
  let simplices = map IM.elems (getDelaunayTiles tessellation)
      integrals = map (integratePolynomialOnSimplex p) simplices 
  return $ sum integrals

