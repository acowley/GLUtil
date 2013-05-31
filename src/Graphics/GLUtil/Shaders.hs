-- |Utilities for working with fragment and vertex shader programs.
module Graphics.GLUtil.Shaders (loadShader, loadGeoShader,
                                linkShaderProgram,
                                linkShaderProgramWith, 
                                linkGeoProgram, linkGeoProgramWith,
                                namedUniform, 
                                uniformScalar, uniformVec, uniformMat, 
                                namedUniformMat, uniformGLMat4) where
import Control.Applicative ((<$>))
import Control.Monad (unless, replicateM)
import Foreign.C.String (peekCStringLen, withCStringLen)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (withArray)
import Foreign.Storable (peek)
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import Graphics.GLUtil.GLError
import Foreign.Ptr (Ptr)
import Unsafe.Coerce (unsafeCoerce)

-- This module is based on the ogl2brick example in the GLUT package.

-- |Load a shader program from a file.
loadShader :: Shader s => FilePath -> IO s
loadShader filePath = do
  src <- readFile filePath
  [shader] <- genObjectNames 1
  shaderSource shader $= [src]
  compileShader shader
  printError
  ok <- get (compileStatus shader)
  infoLog <- get (shaderInfoLog shader)
  unless (null infoLog)
         (mapM_ putStrLn 
                ["Shader info log for '" ++ filePath ++ "':", infoLog, ""])
  unless ok $ do
    deleteObjectNames [shader]
    ioError (userError "shader compilation failed")
  return shader

-- |Specialized loading for geometry shaders that are not yet fully
-- supported by the Haskell OpenGL package.
loadGeoShader :: FilePath -> IO GeometryShader
loadGeoShader filePath = do
  src <- readFile filePath
  [shader@(GeometryShader gid)] <- genObjectNames 1
  setSource gid src
  glCompileShader gid
  printError
  ok <- alloca $ \buf -> do
          glGetShaderiv gid gl_COMPILE_STATUS buf
          fmap (> 0) (peek buf)
  infoLogLen <- alloca $ \ptr -> do glGetShaderiv gid gl_INFO_LOG_LENGTH ptr
                                    peek ptr
  infoLog <- alloca $ \len ->
               allocaBytes (fromIntegral infoLogLen) $ \chars -> do 
                 glGetShaderInfoLog gid infoLogLen len chars
                 len' <- fromIntegral <$> peek len
                 peekCStringLen (chars, len')
  unless (null infoLog)
         (mapM_ putStrLn 
                ["Shader info log for '" ++ filePath ++ "':", infoLog, ""])
  unless ok $ do
    deleteObjectNames [shader]
    ioError (userError "shader compilation failed")
  return shader
  where setSource i src = 
          do withCStringLen src $ \(charBuf,len)-> do
               withArray [charBuf] $ \charBufsBuf ->
                 withArray [fromIntegral len] $ \lengthsBuf ->
                   glShaderSource i 1 charBufsBuf lengthsBuf


-- |Link vertex and fragment shaders into a 'Program'.
linkShaderProgram :: [VertexShader] -> [FragmentShader] -> IO Program
linkShaderProgram vs fs = linkShaderProgramWith vs fs (\_ -> return ())

-- |Link vertex and fragment shaders into a 'Program'. The supplied
-- 'IO' action is run after attaching shader objects to the new
-- program, but before linking. This supports the use of
-- 'bindFragDataLocation' to map fragment shader outputs.
linkShaderProgramWith :: [VertexShader] -> [FragmentShader]
                      -> (Program -> IO ()) -> IO Program
linkShaderProgramWith vs fs m = linkGeoProgramWith vs [] fs m

newtype GeometryShader = GeometryShader { geometryShaderID :: GLuint }
  deriving (Eq,Ord,Show)

instance ObjectName GeometryShader where
  genObjectNames n = replicateM n $ 
                     fmap GeometryShader (glCreateShader gl_GEOMETRY_SHADER)
  deleteObjectNames = mapM_ (glDeleteShader . geometryShaderID)
  isObjectName = fmap (> 0) . glIsShader . geometryShaderID

-- |Link vertex, geometry, and fragment shaders into a 'Program'.
linkGeoProgram :: [VertexShader] -> [GeometryShader] -> [FragmentShader]
               -> IO Program
linkGeoProgram vs gs fs = linkGeoProgramWith vs gs fs (\_ -> return ())

-- |Link vertex, geometry, and fragment shaders into a 'Program'. The
-- supplied 'IO' action is run after attaching shader objects to the
-- new program, but before linking. This supports the use of
-- 'bindFragDataLocation' to map fragment shader outputs.
linkGeoProgramWith :: [VertexShader] -> [GeometryShader] -> [FragmentShader]
                   -> (Program -> IO ()) -> IO Program
linkGeoProgramWith vs gs fs m = do
  [prog] <- genObjectNames 1
  attachedShaders prog $= (vs, fs)
  mapM_ (glAttachShader (unsafeCoerce prog) . geometryShaderID) gs
  m prog
  linkProgram prog
  printError
  ok <- get (linkStatus prog)
  infoLog <- get (programInfoLog prog)
  unless (null infoLog)
         (mapM_ putStrLn ["Program info log:", infoLog, ""])
  unless ok $ do
    deleteObjectNames [prog]
    ioError (userError "GLSL linking failed")
  return prog

-- |Work with a named uniform shader parameter. Note that this looks
-- up the variable name on each access, so uniform parameters that
-- will be accessed frequently should instead be resolved to a
-- 'UniformLocation'.
namedUniform :: (Uniform a) => String -> StateVar a
namedUniform name = makeStateVar (loc >>= get) (\x -> loc >>= ($= x))
  where loc = do Just p <- get currentProgram
                 l <- get (uniformLocation p name)
                 printError
                 return $ uniform l

-- Allocate an OpenGL matrix from a nested list matrix, and pass a
-- pointer to that matrix to an 'IO' action.
withHMatrix :: [[GLfloat]] -> (Ptr GLfloat -> IO a) -> IO a
withHMatrix lstMat m = do
    mat <- newMatrix RowMajor (concat lstMat) :: IO (GLmatrix GLfloat)
    withMatrix mat (\_ -> m)

-- Not all raw uniform setters are wrapped by the OpenGL interface,
-- but the UniformLocation newtype is still helpful for type
-- discipline.
unUL :: UniformLocation -> GLint
unUL = unsafeCoerce

-- |Set a 'UniformLocation' to a scalar value.
uniformScalar :: UniformComponent a => UniformLocation -> SettableStateVar a
uniformScalar loc = makeSettableStateVar $ (uniform loc $=) . Index1

-- |Set a 'UniformLocation' from a list representation of a
-- low-dimensional vector of 'GLfloat's. Only 2, 3, and 4 dimensional
-- vectors are supported.
uniformVec :: UniformLocation -> SettableStateVar [GLfloat]
uniformVec loc = makeSettableStateVar aux
  where aux [x,y] = glUniform2f loc' x y
        aux [x,y,z] = glUniform3f loc' x y z
        aux [x,y,z,w] = glUniform4f loc' x y z w
        aux _ = ioError . userError $
                "Only 2, 3, and 4 dimensional vectors are supported"
        loc' = unUL loc

-- |Set a named uniform shader parameter from a nested list matrix
-- representation. Only 3x3 and 4x4 matrices are supported.
namedUniformMat :: String -> SettableStateVar [[GLfloat]]
namedUniformMat var = makeSettableStateVar (\m -> loc >>= ($= m) . uniformMat)
  where loc = do Just p <- get currentProgram
                 location <- get (uniformLocation p var)
                 printError
                 return location

-- |Set a uniform shader location from a nested list matrix
-- representation. Only 3x3 and 4x4 matrices are supported.
uniformMat :: UniformLocation -> SettableStateVar [[GLfloat]]
uniformMat loc = makeSettableStateVar aux
  where aux mat = do withHMatrix mat $ \ptr ->
                       case length mat of
                         4 -> glUniformMatrix4fv loc' 1 1 ptr
                         3 -> glUniformMatrix3fv loc' 1 1 ptr
                         _ -> ioError . userError $ 
                              "Only 3x3 and 4x4 matrices are supported"
        loc' = unUL loc

-- |Set a uniform shader location with a 4x4 'GLmatrix'.
uniformGLMat4 :: UniformLocation -> SettableStateVar (GLmatrix GLfloat)
uniformGLMat4 loc = makeSettableStateVar aux
  where aux m = withMatrix m $ \_ -> glUniformMatrix4fv loc' 1 1
        loc' = unUL loc
