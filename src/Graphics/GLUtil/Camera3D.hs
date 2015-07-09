-- | A 'Camera' represents a coordinate frame into which 3D points may
-- be transformed. For rendering purposes, it is often helpful to
-- combine a transformation matrix computed from a 'Camera' by
-- 'camMatrix' -- that transforms points into the camera's coordinate
-- frame -- with a perspective projection matrix, as created by
-- 'projectionMatrix'.
module Graphics.GLUtil.Camera3D
  (-- * Camera movement
   Camera(..), panRad, pan, tiltRad, tilt, rollRad, roll, dolly,
   panGlobalRad, panGlobal, tiltGlobalRad, tiltGlobal, rollGlobalRad,
   rollGlobal,
   -- * Camera initialization
   rosCamera, fpsCamera,
   -- * Matrices
   projectionMatrix, orthoMatrix, camMatrix,
   -- * Miscellaneous
   deg2rad) where
import           Linear            (Conjugate (conjugate), Epsilon, V3 (..),
                                    V4 (..))
import           Linear.Matrix     (M44, mkTransformation)
import           Linear.Quaternion (Quaternion, axisAngle, rotate)

-- | A 'Camera' may be translated and rotated to provide a coordinate
-- frame into which 3D points may be transformed.
data Camera a = Camera { forward     :: V3 a
                       , upward      :: V3 a
                       , rightward   :: V3 a
                       , orientation :: Quaternion a
                       , location    :: V3 a }

-- | Pan a camera view (turn side-to-side) by an angle given in
-- radians. Panning is about the camera's up-axis (e.g. the positive
-- Y axis for 'fpsCamera').
panRad :: (Epsilon a, RealFloat a) => a -> Camera a -> Camera a
panRad theta c = c { orientation = orientation c * r }
  where r = axisAngle (upward c) theta

-- | Pan a camera view (turn side-to-side) by an angle given in
-- degrees. Panning is about the camera's up-axis (e.g. the positive
-- Y axis for 'fpsCamera').
pan :: (Epsilon a, RealFloat a) => a -> Camera a -> Camera a
pan = panRad . deg2rad

-- | Pan a camera view (turn side-to-side) by an angle given in
-- radians. Panning is about the world's up-axis as captured by the
-- initial camera state (e.g. the positive Y axis for 'fpsCamera').
panGlobalRad :: (Epsilon a, RealFloat a) => a -> Camera a -> Camera a
panGlobalRad theta c = c { orientation = r * orientation c }
  where r = axisAngle (upward c) theta

-- | Pan a camera view (turn side-to-side) by an angle given in
-- degrees. Panning is about the world's up-axis as captured by the
-- initial camera state (e.g. the positive Y axis for 'fpsCamera').
panGlobal :: (Epsilon a, RealFloat a) => a -> Camera a -> Camera a
panGlobal = panGlobalRad . deg2rad

-- | Tilt a camera view (up-and-down) by an angle given in
-- radians. Tilting is about the camera's horizontal axis (e.g. the
-- positive X axis for 'fpsCamera').
tiltRad :: (Epsilon a, RealFloat a) => a -> Camera a -> Camera a
tiltRad theta c = c { orientation = orientation c * r }
  where r = axisAngle (rightward c) theta

-- | Tilt a camera view (up-and-down) by an angle given in degrees.
-- Tilting is about the camera's horizontal axis (e.g. the positive X
-- axis for 'fpsCamera').
tilt :: (Epsilon a, RealFloat a) => a -> Camera a -> Camera a
tilt = tiltRad . deg2rad

-- | Tilt a camera view (up-and-down) by an angle given in
-- radians. Tilting is about the world's horizontal axis as captured by
-- the initial camera state (e.g. the positive X axis for 'fpsCamera').
tiltGlobalRad :: (Epsilon a, RealFloat a) => a -> Camera a -> Camera a
tiltGlobalRad theta c = c { orientation = r * orientation c }
  where r = axisAngle (rightward c) theta

-- | Tilt a camera view (up-and-down) by an angle given in degrees.
-- Tilting is about the world's horizontal axis as captured by the
-- initial camera state (e.g. the positive X axis for 'fpsCamera').
tiltGlobal :: (Epsilon a, RealFloat a) => a -> Camera a -> Camera a
tiltGlobal = tiltGlobalRad . deg2rad

-- | Roll a camera view about its view direction by an angle given in
-- radians. Rolling is about the camera's forward axis (e.g. the
-- negative Z axis for 'fpsCamera').
rollRad :: (Epsilon a, RealFloat a) => a -> Camera a -> Camera a
rollRad theta c = c { orientation = orientation c * r }
  where r = axisAngle (forward c) theta

-- | Roll a camera view about its view direction by an angle given in
-- degrees. Rolling is about the camera's forward axis (e.g. the
-- negative Z axis for 'fpsCamera').
roll :: (Epsilon a, RealFloat a) => a -> Camera a -> Camera a
roll = rollRad . deg2rad

-- | Roll a camera view about its view direction by an angle given in
-- radians. Rolling is about the world's forward axis as captured by
-- the initial camera state (e.g. the negative Z axis for 'fpsCamera').
rollGlobalRad :: (Epsilon a, RealFloat a) => a -> Camera a -> Camera a
rollGlobalRad theta c = c { orientation = r * orientation c }
  where r = axisAngle (forward c) theta

-- | Roll a camera view about its view direction by an angle given in
-- degrees. Rolling is about the world's forward axis as captured by the
-- initial camera state (e.g. the negative Z axis for 'fpsCamera').
rollGlobal :: (Epsilon a, RealFloat a) => a -> Camera a -> Camera a
rollGlobal = rollGlobalRad . deg2rad

-- | Translate a camera's position by the given vector.
dolly :: (Conjugate a, Epsilon a, RealFloat a) => V3 a -> Camera a -> Camera a
dolly t c = c { location = location c + t' }
  where t' = orientation c `rotate` t

-- | Convert degrees to radians.
deg2rad :: RealFloat a => a -> a
deg2rad x = x * pi / 180

-- | A camera at the origin with its up-axis coincident with the
-- positive Z axis. This is the convention used by the ROS robotics
-- platform.
rosCamera :: (Epsilon a, RealFloat a) => Camera a
rosCamera = Camera (V3 1 0 0) (V3 0 0 1) (V3 0 1 0) 1 0

-- | A camera at the origin with its up-axis coincident with the
-- positive Y axis. This is the convention used by "first-person
-- shooter" (fps) video games.
fpsCamera :: (Epsilon a, RealFloat a) => Camera a
fpsCamera = Camera (V3 0 0 (-1)) (V3 0 1 0) (V3 1 0 0) 1 0

-- | @projectionMatrix fov aspect near far@ produces a perspective
-- projection matrix with the specified vertical field of view (FOV),
-- given in radians, aspect ratio, and near and far clipping planes.
projectionMatrix :: (Conjugate a, Epsilon a, RealFloat a)
                 => a -> a -> a -> a -> M44 a
projectionMatrix fovy aspect near far =
  V4 (V4 (focal / aspect) 0 0 0)
     (V4 0 focal 0 0)
     (V4 0 0 ((far+near) / (near - far)) ((2*far*near) / (near - far)))
     (V4 0 0 (-1) 0)
  where focal = 1 / tan (fovy * 0.5)

-- | @orthoMatrix left right top bottom near far@ produces a parallel
-- projection matrix with the specified left, right, top, bottom, near and
-- far clipping planes.
orthoMatrix :: (Num a, Fractional a) => a -> a -> a -> a -> a -> a -> M44 a
orthoMatrix left right top bottom near far =
    V4 (V4 (2/(right-left)) 0 0 (-(right+left)/(right-left)) )
       (V4 0 (2/(top-bottom)) 0 (-(top+bottom)/(top-bottom)) )
       (V4 0 0 (-2/(far-near)) (-(far+near)/(far-near)) )
       (V4 0 0 0 1)

-- | Produce a transformation matrix from a 'Camera'. This matrix
-- transforms homogenous points into the camera's coordinate frame.
camMatrix :: (Conjugate a, Epsilon a, RealFloat a) => Camera a -> M44 a
camMatrix c = mkTransformation q (rotate q . negate . location $ c)
  where q = conjugate $ orientation c

{-
-- | A lens for the fourth column of a matrix.
translation' :: (R3 t, R4 v, Functor f)
            => (V3 a -> f (V3 a)) -> t (v a) -> f (t (v a))
translation' f m = fmap (\(V3 x y z) -> m & _x._w .~ x & _y._w .~ y & _z._w .~ z)
                        (f (fmap (^. _w) (m ^. _xyz)))
-}
