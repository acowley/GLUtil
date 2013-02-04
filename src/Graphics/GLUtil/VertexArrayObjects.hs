-- | A thin layer over OpenGL 3.1+ vertex array objects.
module Graphics.GLUtil.VertexArrayObjects (makeVAO, withVAO, VAO) where
import Graphics.Rendering.OpenGL

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
withVAO :: VertexArrayObject -> IO () -> IO ()
withVAO vao useIt = do bindVertexArrayObject $= Just vao
                       useIt
                       bindVertexArrayObject $= Nothing
