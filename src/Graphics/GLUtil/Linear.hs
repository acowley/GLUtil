{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
-- |Support for writing "Linear" types to uniform locations in
-- shader programs.
module Graphics.GLUtil.Linear (AsUniform(..), GLComponent(..)) where
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable)
import Graphics.Rendering.OpenGL (UniformLocation)
import Graphics.Rendering.OpenGL.Raw.Core31
import Linear
import Unsafe.Coerce (unsafeCoerce)

-- | A type class for things we can write to uniform locations in
-- shader programs.
class AsUniform t where
  asUniform :: t -> UniformLocation -> IO ()

getUL :: UniformLocation -> GLint
getUL = unsafeCoerce

class Storable a => GLComponent a where
  uniform1v    :: UniformLocation -> GLsizei -> Ptr a -> IO ()
  uniform2v    :: UniformLocation -> GLsizei -> Ptr a -> IO ()
  uniform3v    :: UniformLocation -> GLsizei -> Ptr a -> IO ()
  uniform4v    :: UniformLocation -> GLsizei -> Ptr a -> IO ()

instance GLComponent GLint where
  uniform1v = glUniform1iv . getUL
  uniform2v = glUniform2iv . getUL
  uniform3v = glUniform3iv . getUL
  uniform4v = glUniform4iv . getUL
  
instance GLComponent GLuint where
  uniform1v = glUniform1uiv . getUL
  uniform2v = glUniform2uiv . getUL
  uniform3v = glUniform3uiv . getUL
  uniform4v = glUniform4uiv . getUL

instance GLComponent GLfloat where
  uniform1v = glUniform1fv . getUL
  uniform2v = glUniform2fv . getUL
  uniform3v = glUniform3fv . getUL
  uniform4v = glUniform4fv . getUL

castVecComponent :: Ptr (t a) -> Ptr a
castVecComponent = castPtr

castMatComponent :: Ptr (t (f a)) -> Ptr a
castMatComponent = castPtr

instance AsUniform GLint where
  x `asUniform` loc = with x $ uniform1v loc 1

instance AsUniform GLuint where
  x `asUniform` loc = with x $ uniform1v loc 1

instance AsUniform GLfloat where
  x `asUniform` loc = with x $ uniform1v loc 1

instance GLComponent a => AsUniform (V2 a) where
  v `asUniform` loc = with v $ uniform2v loc 1 . castVecComponent

instance GLComponent a => AsUniform (V3 a) where
  v `asUniform` loc = with v $ uniform3v loc 1 . castVecComponent

instance GLComponent a => AsUniform (V4 a) where
  v `asUniform` loc = with v $ uniform4v loc 1 . castVecComponent

instance AsUniform (M22 GLfloat) where
  m `asUniform` loc = with m
                    $ glUniformMatrix2fv (getUL loc) 1 0 . castMatComponent

instance AsUniform (M33 GLfloat) where
  m `asUniform` loc = with m
                    $ glUniformMatrix3fv (getUL loc) 1 0 . castMatComponent

instance AsUniform (M44 GLfloat) where
  m `asUniform` loc = with m
                    $ glUniformMatrix4fv (getUL loc) 1 0 . castMatComponent

-- In order to support uniform arrays, we support lists of vectors and
-- matrices. We do this by tying the dimensionality of each supported
-- finite dimensional vector to a GL call.

class HasComponents t where
  uniformv :: GLComponent a
           => t a -> UniformLocation -> GLsizei -> Ptr a -> IO ()

instance HasComponents V2 where uniformv _ = uniform2v

instance HasComponents V3 where uniformv _ = uniform3v

instance HasComponents V4 where uniformv _ = uniform4v

instance forall a t. 
         (AsUniform (t a), GLComponent a, HasComponents t, Storable (t a))
         => AsUniform [t a] where
  l `asUniform` loc = withArray l
                    $ uniformv (undefined::t a) loc (fromIntegral $ length l)
                    . castVecComponent

instance forall a f t. 
         (AsUniform (f a), GLComponent a, HasComponents t, Storable (t (f a)))
         => AsUniform [t (f a)] where
  l `asUniform` loc = withArray l
                    $ uniformv (undefined::t a) loc (fromIntegral $ length l)
                    . castMatComponent
