-- | A port of the code presented at [Modern OpenGL with
-- Haskell](http://www.arcadianvisions.com/blog/?p=224) to use the
-- GLFW-b package.
import Prelude hiding (init)
import Control.Applicative
import Control.Monad (unless, when)
import Data.Maybe (isNothing)
import Foreign.Storable (sizeOf)
import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import System.FilePath ((</>))
import TGA -- Small library for TGA file handling

-- | A value to carry around a shader program and its parameters.
data Shaders = Shaders { getProgram        :: Program
                       , fadeFactorU    :: UniformLocation
                       , texturesU      :: [UniformLocation] 
                       , positionA      :: AttribLocation }

-- | The resources used for drawing our scene.
data Resources = Resources { vertexBuffer  :: BufferObject
                           , elementBuffer :: BufferObject
                           , textures      :: [TextureObject]
                           , shaders       :: Shaders
                           , fadeFactor    :: GLfloat }

-- | Geometry data is a list of four 2D vertices.
vertexBufferData :: [GLfloat]
vertexBufferData = [-1, -1, 1, -1, -1, 1, 1, 1]

-- | Load a texture and set some texturing parameters.
makeTexture :: FilePath -> IO TextureObject
makeTexture filename = 
    do (width,height,pixels) <- readTGA filename
       tex <- loadTexture $ texInfo width height TexBGR pixels
       textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
       textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
       textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
       return tex

-- | Load and compile our GLSL program, and pull out the parameters we
-- want.
initShaders :: IO Shaders
initShaders = do vs <- loadShader VertexShader $ "shaders" </> "hello-gl.vert"
                 fs <- loadShader FragmentShader $ "shaders" </> "hello-gl.frag"
                 p <- linkShaderProgram [vs,fs]
                 Shaders p
                   <$> get (uniformLocation p "fade_factor")
                   <*> mapM (get . uniformLocation p)
                         ["textures[0]", "textures[1]"]
                   <*> get (attribLocation p "position")

-- | Load our geometry and textures into OpenGL.
makeResources :: IO Resources
makeResources =  Resources
             <$> makeBuffer ArrayBuffer vertexBufferData
             <*> makeBuffer ElementArrayBuffer [0..3::GLuint]
             <*> mapM (makeTexture . ("images" </>)) 
                      ["hello1.tga", "hello2.tga"]
             <*> initShaders
             <*> pure 0.0

-- | Bind textures to GLSL samplers.
setupTexturing :: Resources -> IO ()
setupTexturing r = let [t1, t2] = textures r
                       [tu1, tu2] = texturesU (shaders r)
                   in do activeTexture $= TextureUnit 0
                         textureBinding Texture2D $= Just t1
                         uniform tu1 $= Index1 (0::GLint)
                         activeTexture $= TextureUnit 1
                         textureBinding Texture2D $= Just t2
                         uniform tu2 $= Index1 (1::GLint)

-- | Bind the geometry array and element buffers.
setupGeometry :: Resources -> IO ()
setupGeometry r = let posn = positionA (shaders r)
                      stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
                      vad = VertexArrayDescriptor 2 Float stride offset0
                  in do bindBuffer ArrayBuffer   $= Just (vertexBuffer r)
                        vertexAttribPointer posn $= (ToFloat, vad)
                        vertexAttribArray posn   $= Enabled
                        bindBuffer ElementArrayBuffer $= Just (elementBuffer r)

-- | Set drawing parameters that won't change during execution.
drawInit :: Resources -> IO ()
drawInit r = do clearColor $= Color4 1 1 1 1
                clear [ColorBuffer]
                currentProgram $= Just (getProgram (shaders r))
                setupTexturing r
                setupGeometry r

draw :: Resources -> IO ()
draw r = do uniform (fadeFactorU (shaders r)) $= Index1 (fadeFactor r)
            drawElements TriangleStrip 4 UnsignedInt offset0

animate :: Resources -> IO Resources
animate r = do Just seconds <- getTime
               let fade = sin seconds * 0.5 + 0.5
               return r { fadeFactor = realToFrac fade }

main :: IO ()
main = do ok <- init
          when (not ok) (error "Error initializing GLFW!")
          windowHint $ WindowHint'RefreshRate 100
          m@(~(Just w)) <- createWindow 500 500 "Chapter 2" Nothing Nothing
          when (isNothing m) (error "Couldn't create window!")
          makeContextCurrent m
          setWindowTitle w "Chapter 2"
          r0 <- makeResources
          drawInit r0
          let keyIsPressed k = (== KeyState'Pressed) <$> getKey w k
              go r = do draw r
                        swapBuffers w
                        pollEvents
                        keyIsPressed Key'Escape >>= flip unless (animate r >>= go)
          go r0
