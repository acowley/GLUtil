-- | A thin layer over OpenGL 3.1+ vertex array objects.
module Graphics.GLUtil.VertexArrayObjects
  (makeVAO, withVAO, deleteVAO, deleteVAOs, VAO) where
import Graphics.Rendering.OpenGL
import Graphics.GL.Core31 (glDeleteVertexArrays)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (with)
import Unsafe.Coerce (unsafeCoerce)

-- |Short alias for 'VertexArrayObject'.
type VAO = VertexArrayObject

-- |Allocate a 'VertexArrayObject', and initialize it with the
-- provided action. This action should bind the buffer data, index
-- data (if necessary), and setup vertex attributes.
makeVAO :: IO () -> IO VertexArrayObject
makeVAO setup = do [vao] <- genObjectNames 1
                   bindVertexArrayObject $= Just vao
                   setup
                   bindVertexArrayObject $= Nothing
                   return vao

-- |Run an action with the given 'VertexArrayObject' bound.
withVAO :: VertexArrayObject -> IO r -> IO r
withVAO vao useIt = do bindVertexArrayObject $= Just vao
                       r <- useIt
                       bindVertexArrayObject $= Nothing
                       return r

-- | Delete a 'VertexArrayObject'.
deleteVAO :: VertexArrayObject -> IO ()
deleteVAO vao = with (vaoID vao) $ glDeleteVertexArrays 1
  where vaoID = unsafeCoerce :: VertexArrayObject -> GLuint

-- | Delete a list of 'VertexArrayObject's.
deleteVAOs :: [VertexArrayObject] -> IO ()
deleteVAOs vaos = withArrayLen (map vaoID vaos) $
                    glDeleteVertexArrays . fromIntegral
  where vaoID = unsafeCoerce :: VertexArrayObject -> GLuint
