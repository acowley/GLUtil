{-# LANGUAGE CPP #-}
-- |Convenience interface for working with GLSL shader
-- programs. Provides an interface for setting attributes and
-- uniforms.
module Graphics.GLUtil.ShaderProgram 
  (-- * The ShaderProgram type
   ShaderProgram(..), 
   -- * Simple shader programs utilizing a vertex shader and a fragment shader
   simpleShaderProgram, simpleShaderProgramWith, simpleShaderExplicit,
   simpleShaderProgramBS, simpleShaderProgramWithBS, simpleShaderExplicitBS,
   loadShaderFamily,
   -- * Explicit shader loading
   loadShaderProgram, loadShaderProgramWith,
   loadShaderProgramBS, loadShaderProgramWithBS,
   -- * Working with ShaderProgram parameters
   getAttrib, enableAttrib, setAttrib, setUniform, getUniform) where
import Prelude hiding (lookup)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import qualified Data.ByteString as BS
import Data.List (find, findIndex, isSuffixOf)
import Data.Map.Strict (Map, fromList, lookup)
import Data.Maybe (isJust, isNothing, catMaybes)
import Graphics.GLUtil.Linear (AsUniform(..))
import Graphics.GLUtil.Shaders (loadShader, linkShaderProgram,
                                linkShaderProgramWith, loadShaderBS)
import Graphics.GLUtil.GLError (throwError)
import Graphics.Rendering.OpenGL
import System.Directory (doesFileExist)
import System.FilePath ((<.>))

-- |Representation of a GLSL shader program that has been compiled and
-- linked.
data ShaderProgram = 
  ShaderProgram { attribs  :: Map String (AttribLocation, VariableType)
                , uniforms :: Map String (UniformLocation, VariableType)
                , program  :: Program }

-- |Load a 'ShaderProgram' from a vertex and fragment shader source
-- files. the third argument is a tuple of the attribute names and
-- uniform names that will be set in this program. If all attributes
-- and uniforms are desired, consider using 'loadShaderProgram'.
simpleShaderExplicit :: FilePath -> FilePath -> ([String],[String])
                     -> IO ShaderProgram
simpleShaderExplicit = simpleShaderExplicit' loadShader

simpleShaderExplicitBS :: BS.ByteString -> BS.ByteString -> ([String],[String])
                     -> IO ShaderProgram
simpleShaderExplicitBS = simpleShaderExplicit' (loadShaderBS "ByteString literal")

simpleShaderExplicit' :: (ShaderType -> a -> IO Shader)
                      -> a -> a -> ([String],[String])
                      -> IO ShaderProgram
simpleShaderExplicit' load vsrc fsrc names =
  do vs <- load VertexShader vsrc
     fs <- load FragmentShader fsrc
     p <- linkShaderProgram [vs,fs]
     throwError
     (attrs,unis) <- getExplicits p names
     return $ ShaderProgram (fromList attrs) (fromList unis) p

-- |Load a 'ShaderProgram' from a vertex shader source file and a
-- fragment shader source file. The active attributes and uniforms in
-- the linked program are recorded in the 'ShaderProgram'.
simpleShaderProgram :: FilePath -> FilePath -> IO ShaderProgram
simpleShaderProgram vsrc fsrc = 
  simpleShaderProgramWith vsrc fsrc (\_ -> return ())

-- |Load a 'ShaderProgram' from vertex and fragment shader source
-- strings. The active attributes and uniforms in the linked program
-- are recorded in the 'ShaderProgram'.
simpleShaderProgramBS :: BS.ByteString -> BS.ByteString -> IO ShaderProgram
simpleShaderProgramBS vsrc fsrc =
  simpleShaderProgramWithBS vsrc fsrc (\_ -> return ())

-- |Load a 'ShaderProgram' from a vertex shader source file and a
-- fragment shader source file. The active attributes and uniforms in
-- the linked program are recorded in the 'ShaderProgram'. The
-- supplied 'IO' function is applied to the new program after shader
-- objects are attached to the program, but before linking. This
-- supports the use of 'bindFragDataLocation' to map fragment shader
-- outputs.
simpleShaderProgramWith :: FilePath -> FilePath -> (Program -> IO ())
                        -> IO ShaderProgram
simpleShaderProgramWith vsrc fsrc m = 
  loadShaderProgramWith [(VertexShader, vsrc), (FragmentShader, fsrc)] m

-- |Load a 'ShaderProgram' from vertex and fragment shader source
-- strings. See 'simpleShaderProgramWith' for more information.
simpleShaderProgramWithBS :: BS.ByteString -> BS.ByteString
                          -> (Program -> IO ()) -> IO ShaderProgram
simpleShaderProgramWithBS vsrc fsrc m =
  loadShaderProgramWithBS [(VertexShader, vsrc), (FragmentShader, fsrc)] m

loadShaderProgramWith :: [(ShaderType, FilePath)] -> (Program -> IO ())
                      -> IO ShaderProgram
loadShaderProgramWith = loadShaderProgramWith' loadShader

loadShaderProgramWithBS :: [(ShaderType, BS.ByteString)] -> (Program -> IO ())
                        -> IO ShaderProgram
loadShaderProgramWithBS = loadShaderProgramWith' (loadShaderBS "ByteString literal")

-- | Helper for @load*Program*@ variants.
loadShaderProgramWith' :: (ShaderType -> a -> IO Shader)
                       -> [(ShaderType, a)] -> (Program -> IO ())
                       -> IO ShaderProgram
loadShaderProgramWith' load sources m =
  do p <- mapM (uncurry load) sources >>= flip linkShaderProgramWith m
     throwError
     (attrs,unis) <- getActives p
     return $ ShaderProgram (fromList attrs) (fromList unis) p

-- |Load a 'ShaderProgram' from a list of individual shader program
-- files. The active attributes and uniforms in the linked program are
-- recorded in the 'ShaderProgram'
loadShaderProgram :: [(ShaderType, FilePath)] -> IO ShaderProgram
loadShaderProgram = flip loadShaderProgramWith (const (return ()))

-- | Load a 'ShaderProgram' from a list of individual shader program
-- source strings. The active attributes and uniforms in the linked program are
-- recorded in the 'ShaderProgram'
loadShaderProgramBS :: [(ShaderType, BS.ByteString)] -> IO ShaderProgram
loadShaderProgramBS = flip loadShaderProgramWithBS (const (return ()))

-- | Load a shader program from vertex, geometry, and fragment shaders
-- that all share the same root file name and the various conventional
-- extensions: \".vert\", \".geom\", and \".frag\". If a specific file
-- doesn't exist, such as a geometry shader, it is skipped. For
-- instance, @loadShaderFamily "simple"@ will load and compile,
-- \"simple.vert\" and \"simple.frag\" if those files exist.
loadShaderFamily :: FilePath -> IO ShaderProgram
loadShaderFamily root = mapM aux shaderTypes >>= loadShaderProgram . catMaybes
  where shaderTypes = [ (VertexShader, "vert")
                      , (GeometryShader, "geom")
                      , (FragmentShader, "frag") ]
        aux (tag, ext) = let f = root <.> ext
                         in doesFileExist f >>= \b -> return $
                              if b then Just (tag, f) else Nothing

-- | Get all attributes and uniforms used by a program. Note that
-- unused parameters may be elided by the compiler, and so will not be
-- considered as active.
getActives :: Program -> 
              IO ( [(String, (AttribLocation, VariableType))]
                 , [(String, (UniformLocation, VariableType))] )
getActives p = 
  (,) <$> (get (activeAttribs p) >>= mapM (aux (attribLocation p)))
      <*> (get (activeUniforms p)
           >>= mapM (aux (uniformLocation p) . on3 trimArray))
  where aux f (_,t,name) = get (f name) >>= \l -> return (name, (l, t))
        on3 f (a,b,c) = (a, b, f c)
        -- An array uniform, foo, is sometimes given the name "foo" and
        -- sometimes the name "foo[0]". We strip off the "[0]" if present.
        trimArray n = if "[0]" `isSuffixOf` n then take (length n - 3) n else n

-- | Get the attribute and uniform locations associated with a list of
-- the names of each.
getExplicits :: Program -> ([String], [String]) ->
                IO ( [(String, (AttribLocation, VariableType))]
                   , [(String, (UniformLocation, VariableType))] )
getExplicits p (anames, unames) = 
  do attrs <- get (activeAttribs p)
     attrs' <- mapM (aux (get . (attribLocation p))) . checkJusts $
               map (\a -> find (\(_,_,n) -> n == a) attrs) anames
     unis <- get (activeUniforms p)
     unis' <- mapM (aux (get . (uniformLocation p))) . checkJusts $
              map (\u -> find (\(_,_,n) -> n == u) unis) unames
     return (attrs', unis')
  where aux f (_,t,n) = f n >>= \l -> return (n, (l,t))
        checkJusts xs
          | all isJust xs = catMaybes xs
          | otherwise = let Just i = findIndex isNothing xs
                        in error $ "Missing GLSL variable: " ++ anames !! i

-- | Set a named uniform parameter associated with a particular shader
-- program.
setUniform :: AsUniform a => ShaderProgram -> String -> a -> IO ()
setUniform sp name = maybe (const (putStrLn warn >> return ()))
                           (flip asUniform . fst)
                           (lookup name $ uniforms sp)
  where warn = "WARNING: uniform "++name++" is not active"

-- | Get the 'UniformLocation' associated with a named uniform
-- parameter.
getUniform :: ShaderProgram -> String -> UniformLocation
getUniform sp n = maybe (error msg) fst . lookup n $ uniforms sp
  where msg = "Uniform "++show n++" is not active"

-- | Set a named vertex attribute's 'IntegerHandling' and
-- 'VertexArrayDescriptor'.
setAttrib :: ShaderProgram -> String -> 
             IntegerHandling -> VertexArrayDescriptor a -> IO ()
setAttrib sp name = maybe (\_ _ -> putStrLn warn >> return ())
                          (\(a,_) -> let vap = vertexAttribPointer a
                                     in \ih vad -> (($= (ih, vad)) vap))
                          (lookup name $ attribs sp)
  where warn = "WARNING: attrib "++name++" is not active"

-- | Get the 'AttribLocation' associated with a named vertex
-- attribute.
getAttrib :: ShaderProgram -> String -> AttribLocation
getAttrib sp n = maybe (error msg) fst . lookup n $ attribs sp
  where msg = "Attrib "++show n++" is not active"

-- | Enable a named vertex attribute.
enableAttrib :: ShaderProgram -> String -> IO ()
enableAttrib sp name = maybe (return ())
                             (($= Enabled) . vertexAttribArray . fst)
                             (lookup name $ attribs sp)
