{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Hash.ECM (hashECM, uncompress) where

-- Elliptic curve multiplication

import Data.Words
import Util.Numeric (modPow)
import Prelude hiding ((^))
import qualified Prelude as P

(^) :: Integer -> Integer -> Integer
(^) = (P.^)
infixr 8 ^

-- | Type synonyms for points in two and three dimensions
type Point2D = (Integer, Integer)
type Point3D = (Integer, Integer, Integer)

-- * Interface
-- ----------------------------------------------------------------------------

-- | Elliptic curve multiplication hashing
hashECM :: Word256 -> (Word256,Word256)
hashECM w | (x,y) <- fastMult genPoint (toInteger w)
          = (fromInteger x, fromInteger y)

uncompress :: Word8 -> Word256 -> (Word256,Word256) -- prefix == 0x02 or 0x03
uncompress prefix w = (w, fromInteger y)
  where
    x = toInteger w
    b = modPow (x*x*x+aconst*x+bconst) ((pconst+1) `shiftR` 2) pconst
    y = if (b + toInteger prefix) `mod` 2 /= 0
          then pconst - b
          else b

-- * Elliptic curve parameters (secp256k1)
-- ----------------------------------------------------------------------------

pconst :: Integer
pconst = 115792089237316195423570985008687907853269984665640564039457584007908834671663

nconst :: Integer
nconst = 115792089237316195423570985008687907852837564279074904382605163141518161494337

aconst :: Integer
aconst = 0

bconst :: Integer
bconst = 7

genPointX, genPointY :: Integer
genPointX = 55066263022277343669578718895168534326250603453777594175500187360389116729240
genPointY = 32670510020758816978083085130507043184471273380659243275938904335757337482424

genPoint :: Point2D
genPoint = (genPointX,genPointY)

-- * Extended Euclidean Algorithm
-- ----------------------------------------------------------------------------

inv :: Integer -> Integer -> Integer
inv 0 _ = 0
inv a n = aux 1 0 (a `mod` n) n
  where
    aux lm hm low high
      | low > 1, let r = high `div` low
      = aux (hm - lm * r) lm (high - low * r) low
      | otherwise = lm `mod` n

-- * Elliptic curve multiplication
-- ----------------------------------------------------------------------------

toJacobian :: Point2D -> Point3D
toJacobian (x,y) = (x,y,1)

fromJacobian :: Point3D -> Point2D
fromJacobian (x,y,z) | nz <- inv z pconst
                     = ((x * nz ^ 2) `mod` pconst, (y * nz ^ 3) `mod` pconst)

-- | Doubling
jacobianDouble :: Point3D -> Point3D
jacobianDouble (_x,0 ,_z) = (0,0,0)
jacobianDouble (px,py,pz) = (nx,ny,nz)
  where
    ysq,s,m  :: Integer -- Auxiliary definitions
    nx,ny,nz :: Integer -- Result in parts

    ysq = (py ^ 2)                               `mod` pconst
    s   = ((px * ysq) `shiftL` 2)                `mod` pconst
    m   = (3 * (px ^ 2) + aconst * (pz ^ 4))     `mod` pconst

    nx = (m ^ 2 - (s `shiftL` 1))                `mod` pconst
    ny = (m * (s - nx) - ((ysq ^ 2) `shiftL` 3)) `mod` pconst
    nz = ((py * pz) `shiftL` 1)                  `mod` pconst

-- | Addition
jacobianAdd :: Point3D -> Point3D -> Point3D
jacobianAdd (_,0,_) q = q
jacobianAdd p (_,0,_) = p
jacobianAdd p@(px,py,pz) (qx,qy,qz)
  | u1 == u2 = if s1 /= s2 then (0,0,1) else jacobianDouble p
  | otherwise
  = let h    = u2 - u1                in
    let r    = s2 - s1                in
    let h2   = (h * h)   `mod` pconst in
    let h3   = (h * h2)  `mod` pconst in
    let u1h2 = (u1 * h2) `mod` pconst in

    let nx = (r ^ 2 - h3 - (u1h2 `shiftL` 1))     `mod` pconst in -- TODO: (`shiftL` 1) instead of (* 2)
    let ny = (r * (u1h2 - nx) - s1 * h3) `mod` pconst in
    let nz = (h * pz * qz)               `mod` pconst in

    (nx,ny,nz)
  where
    u1 = (px * qz ^ 2) `mod` pconst
    u2 = (qx * pz ^ 2) `mod` pconst
    s1 = (py * qz ^ 3) `mod` pconst
    s2 = (qy * pz ^ 3) `mod` pconst

-- | Scalar multiplication
jacobianMult :: Point3D -> Integer -> Point3D
jacobianMult p@(_,y,_) n
  | y == 0 || n == 0      = (0,0,1)
  | n == 1                = p
  | n <  0 || n >= nconst = jacobianMult p (n `mod` nconst)
  | n `mod` 2 == 0        = jacobianDouble (jacobianMult p (n `shiftR` 1))
  | otherwise {- 1 -}     = jacobianAdd (jacobianDouble (jacobianMult p (n `shiftR` 1))) p

fastMult :: Point2D -> Integer -> Point2D
fastMult a n = fromJacobian $ jacobianMult (toJacobian a) n

-- | fastAdd :: Point2D -> Point2D -> Point2D
-- | fastAdd a b = fromJacobian $ jacobianAdd (toJacobian a) (toJacobian b)

-- ----------------------------------------------------------------------------

