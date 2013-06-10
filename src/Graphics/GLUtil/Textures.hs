{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, 
             ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
-- |Utilities for loading texture data.
module Graphics.GLUtil.Textures where
import Control.Monad (forM_)
import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL.GL.VertexArrays as GL
import Data.Array.Storable (StorableArray, withStorableArray)
import Data.ByteString.Internal (ByteString, toForeignPtr)
import Data.Vector.Storable (Vector, unsafeWith)
import Data.Word (Word8, Word16)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Marshal.Array (withArray)

import Graphics.GLUtil.TypeMapping (HasGLType(..))

-- |Pixel format of image data.
data TexColor = TexMono | TexRGB | TexBGR | TexRGBA

-- |A basic texture information record.
data TexInfo a = TexInfo { texWidth  :: GLsizei
                         , texHeight :: GLsizei
                         , texColor  :: TexColor
                         , texData   :: a }

-- |Helper for constructing a 'TexInfo' using Haskell 'Int's for image
-- dimensions.
texInfo :: Int -> Int -> TexColor -> a -> TexInfo a
texInfo w h = TexInfo (fromIntegral w) (fromIntegral h)

-- |Class for containers of texture data.
class HasGLType (Elem a) => IsPixelData a where
  type Elem a
  withPixels :: a -> (Ptr (Elem a) -> IO c) -> IO c

instance HasGLType b => IsPixelData [b] where
  type Elem [b] = b
  withPixels = withArray

instance HasGLType b => IsPixelData (Ptr b) where
  type Elem (Ptr b) = b
  withPixels = flip ($)

instance HasGLType b => IsPixelData (ForeignPtr b) where
  type Elem (ForeignPtr b) = b
  withPixels = withForeignPtr

instance HasGLType b => IsPixelData (StorableArray i b) where
  type Elem (StorableArray i b) = b
  withPixels = withStorableArray

instance HasGLType b => IsPixelData (Vector b) where
  type Elem (Vector b) = b
  withPixels = unsafeWith

instance IsPixelData ByteString where
  type Elem ByteString = Word8
  withPixels b m = aux . toForeignPtr $ b
    where aux (fp,o,_) = withForeignPtr fp $ \p ->
                           m (plusPtr p o)

-- |Wrapper whose 'IsPixelData' instance treats the pointer underlying
-- a 'ByteString' as an array of 'Word16's.
newtype ShortString = ShortString ByteString

instance IsPixelData ShortString where
  type Elem ShortString = Word16
  withPixels (ShortString b) m = aux. toForeignPtr $ b
    where aux (fp,o,_) = withForeignPtr fp $ \p ->
                           m (plusPtr (castPtr p :: Ptr Word16) o)

-- |Create a new 2D texture with data from a 'TexInfo'.
loadTexture :: IsPixelData a => TexInfo a -> IO TextureObject
loadTexture tex = do [obj] <- genObjectNames 1
                     reloadTexture obj tex
                     return obj

-- |Replace a 2D texture's pixel data with data from a 'TexInfo'.
reloadTexture :: forall a. IsPixelData a => 
                 TextureObject -> TexInfo a -> IO ()
reloadTexture obj tex = do textureBinding Texture2D $= Just obj
                           loadTex $ texColor tex
  where loadTex TexMono = case pixelType of
                            GL.UnsignedShort -> loadAux Luminance16 Luminance
                            GL.Float         -> loadAux R32F Red
                            _                -> loadAux Luminance' Luminance
                            
        loadTex TexRGB = loadAux RGBA' RGB
        loadTex TexBGR = loadAux RGBA' BGR
        loadTex TexRGBA = loadAux RGBA' RGBA
        sz = TextureSize2D (texWidth tex) (texHeight tex)
        pixelType = glType (undefined::Elem a)
        loadAux i e = withPixels (texData tex) $ 
                      (texImage2D Nothing NoProxy 0 i sz 0 .
                       PixelData e pixelType)

-- | Set texture coordinate wrapping options for both the 'S' and 'T'
-- dimensions of a 2D texture.
texture2DWrap :: StateVar (Repetition, Clamping)
texture2DWrap = makeStateVar (get (textureWrapMode Texture2D S))
                             (forM_ [S,T] . aux)
  where aux x d = textureWrapMode Texture2D d $= x

-- | Set texture coordinate wrapping options for the 'S', 'T', and 'R'
-- dimensions of a 3D texture.
texture3DWrap :: StateVar (Repetition, Clamping)
texture3DWrap = makeStateVar (get (textureWrapMode Texture2D S))
                             (forM_ [S,T,R] . aux)
  where aux x d = textureWrapMode Texture2D d $= x


-- | Bind each of the given textures to successive texture units at
-- the given 'TextureTarget'.
withTextures :: TextureTarget -> [TextureObject] -> IO a -> IO a
withTextures tt ts m = do mapM_ aux (zip ts [0..])
                          r <- m
                          cleanup 0 ts
                          return r
  where aux (t,i) = do activeTexture $= TextureUnit i
                       textureBinding tt $= Just t
        cleanup _ [] = return ()
        cleanup i (_:ts') = do activeTexture $= TextureUnit i
                               textureBinding Texture2D $= Nothing
                               cleanup (i+1) ts'

-- | Bind each of the given 2D textures to successive texture units.
withTextures2D :: [TextureObject] -> IO a -> IO a
withTextures2D = withTextures Texture2D
