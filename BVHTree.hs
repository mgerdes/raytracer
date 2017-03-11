module BVHTree where

import System.Random
import Data.List

import Hitables
import HitRecord
import Vec3
import Ray

data BVHTree = BVHTree {
    bvhBB :: BoundingBox,
    bvhLeft :: BVHTree,
    bvhRight :: BVHTree
} | BVHTreeLeaf Hitable Hitable deriving Show

data BoundingBox = BoundingBox {
    bbMin :: Vec3, 
    bbMax :: Vec3 
} deriving Show

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing b = b
minMaybe a Nothing = a
minMaybe (Just a) (Just b) = Just (min a b)

boundingBoxIntersect :: BoundingBox -> Ray -> Bool 
boundingBoxIntersect bb ray =
    let (ro_x, ro_y, ro_z) = rayOrigin ray
        (rd_x, rd_y, rd_z) = rayDirection ray
        (min_x, min_y, min_z) = bbMin bb
        (max_x, max_y, max_z) = bbMax bb

        t0_x = min ((min_x - ro_x) / rd_x) ((max_x - ro_x) / rd_x)
        t1_x = max ((min_x - ro_x) / rd_x) ((max_x - ro_x) / rd_x)
        tmin_x = min t0_x t1_x
        tmax_x = max t0_x t1_x

        t0_y = min ((min_y - ro_y) / rd_y) ((max_y - ro_y) / rd_y)
        t1_y = max ((min_y - ro_y) / rd_y) ((max_y - ro_y) / rd_y)
        tmin_y = min t0_y t1_y
        tmax_y = max t0_y t1_y

        t0_z = min ((min_z - ro_z) / rd_z) ((max_z - ro_z) / rd_z)
        t1_z = max ((min_z - ro_z) / rd_z) ((max_z - ro_z) / rd_z)
        tmin_z = min t0_z t1_z
        tmax_z = max t0_z t1_z

        tmin0 = tmin_x
        tmax0 = tmax_x

        tmin1 = max tmin0 tmin_y
        tmax1 = min tmax0 tmax_y

        tmin2 = max tmin1 tmin_z
        tmax2 = min tmax1 tmax_z
    in tmin0 < tmax0 && tmin1 < tmax1 && tmin2 < tmax2

bvhIntersect :: BVHTree -> Ray -> Maybe HitRec 
bvhIntersect BVHTree { bvhBB = bb, bvhLeft = leftTree, bvhRight = rightTree } ray =
    if boundingBoxIntersect bb ray then
        minMaybe (bvhIntersect leftTree ray) (bvhIntersect rightTree ray)
    else Nothing
bvhIntersect (BVHTreeLeaf h0 h1) ray = 
    minMaybe (hitableIntersect h0 ray) (hitableIntersect h1 ray)

createBoundingBox :: Hitable -> BoundingBox
createBoundingBox Sphere { sphereCenter = o, sphereRadius = r } = 
    BoundingBox { bbMin = o <-> (r, r, r), bbMax = o <+> (r, r, r) }   

combineBoundingBoxes :: BoundingBox -> BoundingBox -> BoundingBox
combineBoundingBoxes BoundingBox { bbMin = (min0x, min0y, min0z), bbMax = (max0x, max0y, max0z) } 
                     BoundingBox { bbMin = (min1x, min1y, min1z), bbMax = (max1x, max1y, max1z) } =
    BoundingBox { bbMin = (min min0x min1x, min min0y min1y, min min0z min1z),
                  bbMax = (max max0x max1x, max max0y max1y, max max0z max1z) }

bvhBoundingBox :: BVHTree -> BoundingBox
bvhBoundingBox BVHTree { bvhBB = bb } = bb
bvhBoundingBox (BVHTreeLeaf h0 h1) =  
    let bb0 = createBoundingBox h0
        bb1 = createBoundingBox h1
    in combineBoundingBoxes bb0 bb1

xAxisComp :: Hitable -> Hitable -> Ordering
xAxisComp h0 h1 = 
    let bb0 = createBoundingBox h0
        bb1 = createBoundingBox h1
        (x0, _, _) = bbMin bb0
        (x1, _, _) = bbMin bb1
    in compare x0 x1

yAxisComp :: Hitable -> Hitable -> Ordering
yAxisComp h0 h1 = 
    let bb0 = createBoundingBox h0
        bb1 = createBoundingBox h1
        (_, y0, _) = bbMin bb0
        (_, y1, _) = bbMin bb1
    in compare y0 y1

zAxisComp :: Hitable -> Hitable -> Ordering
zAxisComp h0 h1 = 
    let bb0 = createBoundingBox h0
        bb1 = createBoundingBox h1
        (_, _, z0) = bbMin bb0
        (_, _, z1) = bbMin bb1
    in compare z0 z1

createBVHTree :: [Hitable] -> StdGen -> BVHTree
createBVHTree (h0:[]) _ = BVHTreeLeaf h0 h0
createBVHTree (h0:h1:[]) _ = BVHTreeLeaf h0 h1
createBVHTree hitables r0 = 
    let (t, r1) = randomR (0, 2) r0 :: (Int, StdGen)
        n = length hitables

        comp = if t == 0 then xAxisComp
               else if t == 1 then yAxisComp
               else zAxisComp
        sorted = sortBy comp hitables

        leftBVHTree = createBVHTree (take (div n 2) sorted) r1 
        rightBVHTree = createBVHTree (drop (div n 2) sorted) r1

        leftBoundingBox = bvhBoundingBox leftBVHTree
        rightBoundingBox = bvhBoundingBox rightBVHTree
        boundingBox = combineBoundingBoxes leftBoundingBox rightBoundingBox 
    in BVHTree boundingBox leftBVHTree rightBVHTree
