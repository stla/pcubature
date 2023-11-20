{-|
Module      : Numeric.Integration.PolyhedralCubature
Description : Multiple integration over convex polytopes.
Copyright   : (c) Stéphane Laurent, 2023
License     : GPL-3
Maintainer  : laurent_step@outlook.fr

Evaluation of integrals over a convex polytope. See README for examples.
-}
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

-- | Integral of a multivariate function over a convex polytope given by its vertices.
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

-- | Integral of a real-valued function over a convex polytope given by its vertices.
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

-- | Integral of a multivariate function over a convex polytope given by linear inequalities.
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

-- | Integral of a scalar-valued function over a convex polytope given by linear inequalities.
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

-- | Integral of a polynomial over a convex polytope given by its vertices.
integratePolynomialOnPolytope
  :: (RealFrac a, C a)
  => Spray a -- ^ polynomial to be integrated
  -> [[a]]   -- ^ vertices of the polytope to integrate over
  -> IO a
integratePolynomialOnPolytope p vertices = do
  simplices <- delaunay' vertices
  let integrals = map (integratePolynomialOnSimplex p) simplices 
  return $ sum integrals

-- | Integral of a polynomial over a convex polytope given by linear inequalities.
integratePolynomialOnPolytope'
  :: Spray Double        -- ^ polynomial to be integrated
  -> [Constraint Double] -- ^ linear inequalities defining the polytope
  -> IO Double
integratePolynomialOnPolytope' p constraints = do
  vertices <- vertexenum constraints Nothing
  tessellation <- delaunay vertices True False Nothing
  let simplices = map IM.elems (getDelaunayTiles tessellation)
      integrals = map (integratePolynomialOnSimplex p) simplices 
  return $ sum integrals

