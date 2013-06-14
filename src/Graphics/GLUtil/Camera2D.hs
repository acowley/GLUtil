-- | A camera designed for 2D viewing. The camera may be translated
-- perpendicular to its view direction, or rolled about its view
-- direction.
module Graphics.GLUtil.Camera2D 
  (-- * Camera movement
   Camera(..), track, roll, rollRad,
   -- * Camera initialization
   camera2D,
   -- * Math
   camMatrix, deg2rad) where
import Graphics.GLUtil.Camera3D hiding (camMatrix, roll, rollRad)
import qualified Graphics.GLUtil.Camera3D as C
import Linear (Conjugate, Epsilon, V2(..), V3(..), V4(..), M33)

-- | Initialize a camera for 2D rendering.
camera2D :: (Epsilon a, RealFloat a) => Camera a
camera2D = fpsCamera

-- | Move the camera side-to-side or up-and-down as in a tracking shot.
track :: (Conjugate a, Epsilon a, RealFloat a) => V2 a -> Camera a -> Camera a
track (V2 x y) = dolly (V3 x y 0)

-- | Produce a matrix that transforms homogenous 2D points into the
-- camera's coordinate frame.
camMatrix :: (Conjugate a, Epsilon a, RealFloat a) => Camera a -> M33 a
camMatrix = fmap getXYW . getXYW . C.camMatrix
  where getXYW (V4 x y _ w) = V3 x y w

-- | Roll a camera view about its view direction by an angle given in
-- degrees.
roll :: (Epsilon a, RealFloat a) => a -> Camera a -> Camera a
roll = C.roll

-- | Roll a camera view about its view direction by an angle given in
-- radians.
rollRad :: (Epsilon a, RealFloat a) => a -> Camera a -> Camera a
rollRad = C.rollRad
