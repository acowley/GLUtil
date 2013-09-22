This is a Haskell implementation of the ideas presented in [chapter
two](http://duriansoftware.com/joe/An-intro-to-modern-OpenGL.-Chapter-2.1:-Buffers-and-Textures.html)
of Joe Groff's excellent tutorial on modern OpenGL.

This post is a complete program that relies on the [OpenGL](http://hackage.haskell.org/package/OpenGL) 
and [GLUT](http://hackage.haskell.org/package/GLUT)
Haskell packages. It also makes use of some data files: 

- Textures: [hello1.tga](/PostData/GLTutorial/hello1.tga) and
[hello2.tga](/PostData/GLTutorial/hello2.tga)
- Shaders: [hello-gl.frag](/PostData/GLTutorial/hello-gl.frag) and
[hello-gl.vert](/PostData/GLTutorial/hello-gl.vert)

You may copy and paste this post into a `.lhs` file, or
[download it](/PostData/GLTutorial/lesson3b.lhs).

We begin by importing the necessary libraries.

> import Graphics.Rendering.OpenGL
> import Graphics.UI.GLUT
> import Foreign.Storable (sizeOf)
> import Control.Concurrent (threadDelay)
> import Control.Applicative
> import System.FilePath ((</>))

We use a very small [library for loading TGA images](/PostData/GLTutorial/TGA.hs)...

> import TGA

... and a handy [utility library](http://github.com/acowley/GLUtil)
for loading data into OpenGL.

> import Graphics.GLUtil

Optimism dictates that any exit is a successful exit.

> import System.Exit (exitWith, ExitCode(ExitSuccess))

Application state is shared between the rendering and animation
functions with an `IORef`.

> import Data.IORef (IORef, newIORef, readIORef, modifyIORef)

We begin our program by defining the data structures used to carry
program state between frames.

Shader state is a record of compiled shader programs, the uniform
parameters to the shader, and an attribute accessed by the shader.

> data Shaders = Shaders { vertexShader   :: VertexShader
>                        , fragmentShader :: FragmentShader
>                        , getProgram        :: Program
>                        , fadeFactorU    :: UniformLocation
>                        , texturesU      :: [UniformLocation] 
>                        , positionA      :: AttribLocation }

Application state is carried in a record. State, in this case, is made
up of some vertex data, some primitive data (e.g. polygons), two
textures, shader state, and a scalar we use to fade between the two
textures.

> data Resources = Resources { vertexBuffer  :: BufferObject
>                            , elementBuffer :: BufferObject
>                            , textures      :: [TextureObject] 
>                            , shaders       :: Shaders
>                            , fadeFactor    :: GLfloat }

The data that we actually want to render starts life as a list of 2D vertices,

> vertexBufferData :: [GLfloat]
> vertexBufferData = [-1, -1, 1, -1, -1, 1, 1, 1]

and a list of indices into that list,

> elementBufferData :: [GLuint]
> elementBufferData = [0..3]

Textures are prepared by loading them from disk, then setting various
texture rendering modes.

> makeTexture :: FilePath -> IO TextureObject
> makeTexture filename = 
>     do (width,height,pixels) <- readTGA filename
>        texture <- loadTexture $ texInfo width height TexBGR pixels

We set texturing parameters to linear filtering for minification and
magnification, while disabling mip mapping. Texture wrapping is set to
clamp both horizontally and vertically, S and T, respectively.

>        textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
>        textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
>        textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
>        return texture

Now we can load the data we want to render into OpenGL, and track it
using our state record.

Shaders are prepared by loading and compiling the individual vertex
and fragment shaders, then linking them into a program. We then query
the program to get addresses for the uniform parameters and attribute
that we will use to communicate data to the shader program.

> initShaders = do vs <- loadShader VertexShader $ "shaders" </> "hello-gl.vert"
>                  fs <- loadShader FragmentShader $ "shaders" </> "hello-gl.frag"
>                  p <- linkShaderProgram [vs, fs]
>                  Shaders vs fs p
>                    <$> get (uniformLocation p "fade_factor")
>                    <*> mapM (get . uniformLocation p)
>                          ["textures[0]", "textures[1]"]
>                    <*> get (attribLocation p "position")

Our global state record is prepared by creating the buffer objects for
our vertex and index data, loading the image files to be used as
textures, compiling the shader program, and initializing the
`fadeFactor` field to zero.

> makeResources =  Resources
>              <$> makeBuffer ArrayBuffer vertexBufferData
>              <*> makeBuffer ElementArrayBuffer elementBufferData
>              <*> mapM (makeTexture . ("images" </>)) 
>                       ["hello1.tga", "hello2.tga"]
>              <*> initShaders
>              <*> pure 0.0

The interesting part of our program is the function that puts things
on the screen.

One step in rendering is preparing the textures for our shaders. We do
this by activating a texture unit, binding a texture object to the
active texture unit, then setting the uniform sampler2D value in the
fragment shader to refer to the correct texture unit.

> setupTexturing :: Resources -> IO ()
> setupTexturing r = let [t1, t2] = textures r
>                        [tu1, tu2] = texturesU (shaders r)
>                    in do activeTexture $= TextureUnit 0
>                          textureBinding Texture2D $= Just t1
>                          uniform tu1 $= Index1 (0::GLint)
>                          activeTexture $= TextureUnit 1
>                          textureBinding Texture2D $= Just t2
>                          uniform tu2 $= Index1 (1::GLint)

Geometry rendering begins by binding the buffer containing the vertex
data and telling OpenGL how this data is formatted. In our case, each
vertex has two floating point fields.

> setupGeometry :: Resources -> IO ()
> setupGeometry r = let posn = positionA (shaders r)
>                       stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
>                       vad = VertexArrayDescriptor 2 Float stride offset0
>                   in do bindBuffer ArrayBuffer   $= Just (vertexBuffer r)
>                         vertexAttribPointer posn $= (ToFloat, vad)
>                         vertexAttribArray posn   $= Enabled

Finally, drawing is effected by clearing the screen, setting the
`fadeFactor` uniform parameter of our shader program, then drawing our
textured geometry.

> draw :: IORef Resources -> IO ()
> draw r' = do clearColor $= Color4 1 1 1 1
>              clear [ColorBuffer]
>              r <- readIORef r'
>              currentProgram $= Just (getProgram (shaders r))
>              uniform (fadeFactorU (shaders r)) $= Index1 (fadeFactor r)
>              setupTexturing r
>              setupGeometry r
>              bindBuffer ElementArrayBuffer $= Just (elementBuffer r)
>              drawElements TriangleStrip 4 UnsignedInt offset0
>              swapBuffers

The only user interaction we support is exiting when the escape key is
pressed.

> basicKMHandler :: Key -> KeyState -> Modifiers -> Position -> IO ()
> basicKMHandler (Char '\27') Down _ _ = exitWith ExitSuccess
> basicKMHandler _            _    _ _ = return ()

The animation callback limits itself to run at less than 100Hz, then
sets the fade parameter carried in our application state based on
elapsed time.

> animate :: IORef Resources -> IdleCallback
> animate r = do threadDelay 10000
>                milliseconds <- fromIntegral <$> get elapsedTime
>                let fade = sin (milliseconds * 0.001) * 0.5 + 0.5
>                modifyIORef r (\x -> x { fadeFactor = fade })
>                postRedisplay Nothing

Finally, kick GLUT off to open our window and start things going.

> main = do initialDisplayMode $= [DoubleBuffered]
>           initialWindowSize $= Size 500 500
>           (progname,_) <- getArgsAndInitialize
>           createWindow "Chapter 2"
>           r <- makeResources >>= newIORef
>           displayCallback $= draw r
>           idleCallback $= Just (animate r)
>           keyboardMouseCallback $= Just basicKMHandler
>           mainLoop
