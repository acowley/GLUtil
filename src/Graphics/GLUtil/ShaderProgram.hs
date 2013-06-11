-- |Convenience interface for working with GLSL shader
-- programs. Provides an interface for setting attributes and
-- uniforms.
module Graphics.GLUtil.ShaderProgram (ShaderProgram(..), loadShaderProgram, 
                                      loadShaderProgramWith,
                                      loadGeoProgram,
                                      loadGeoProgramWith,
                                      loadShaderExplicit, 
                                      getAttrib, enableAttrib, setAttrib, 
                                      setUniform, getUniform) where
import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Data.List (find, findIndex)
import Data.Map.Strict (Map, fromList, lookup)
import Data.Maybe (isJust, isNothing, catMaybes)
import Graphics.GLUtil.Shaders (loadShader, loadGeoShader, linkShaderProgram,
                                linkGeoProgramWith)
import Graphics.GLUtil.GLError (throwError)
import Graphics.Rendering.OpenGL

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
loadShaderExplicit :: FilePath -> FilePath -> ([String],[String])
                   -> IO ShaderProgram
loadShaderExplicit vsrc fsrc names =
  do vs <- loadShader vsrc
     fs <- loadShader fsrc
     p <- linkShaderProgram [vs] [fs]
     throwError
     (attrs,unis) <- getExplicits p names
     return $ ShaderProgram (fromList attrs) (fromList unis) p

-- |Load a 'ShaderProgram' from a vertex shader source file and a
-- fragment shader source file. The active attributes and uniforms in
-- the linked program are recorded in the 'ShaderProgram'.
loadShaderProgram :: FilePath -> FilePath -> IO ShaderProgram
loadShaderProgram vsrc fsrc = loadShaderProgramWith vsrc fsrc (\_ -> return ())

-- |Load a 'ShaderProgram' from a vertex shader source file, a
-- geometry shader source file, and a fragment shader source file. The
-- active attributes and uniforms in the linked program are recorded
-- in the 'ShaderProgram'.
loadGeoProgram :: FilePath -> FilePath -> FilePath -> IO ShaderProgram
loadGeoProgram vsrc gsrc fsrc = 
  loadGeoProgramWith vsrc gsrc fsrc (\_ -> return ())

-- |Load a 'ShaderProgram' from a vertex shader source file and a
-- fragment shader source file. The active attributes and uniforms in
-- the linked program are recorded in the 'ShaderProgram'. The
-- supplied 'IO' function is applied to the new program after shader
-- objects are attached to the program, but before linking. This
-- supports the use of 'bindFragDataLocation' to map fragment shader
-- outputs.
loadShaderProgramWith :: FilePath -> FilePath -> (Program -> IO ())
                      -> IO ShaderProgram
loadShaderProgramWith vsrc fsrc m = loadProgramWithAux vsrc Nothing fsrc m

-- |Load a 'ShaderProgram' from a vertex shader source file, a
-- geometry shader source file, and a fragment shader source file. The
-- active attributes and uniforms in the linked program are recorded
-- in the 'ShaderProgram'. The supplied 'IO' function is applied to
-- the new program after shader objects are attached to the program,
-- but before linking. This supports the use of 'bindFragDataLocation'
-- to map fragment shader outputs.
loadGeoProgramWith :: FilePath -> FilePath -> FilePath -> (Program -> IO ())
                   -> IO ShaderProgram
loadGeoProgramWith vsrc gsrc fsrc m = loadProgramWithAux vsrc (Just gsrc) fsrc m

-- | Helper for @load*Program*@ variants.
loadProgramWithAux :: FilePath -> Maybe FilePath -> FilePath
                   -> (Program -> IO ()) -> IO ShaderProgram
loadProgramWithAux vsrc gsrc fsrc m =
  do vs <- loadShader vsrc
     gs <- maybe (return []) (fmap (:[]) . loadGeoShader) gsrc
     fs <- loadShader fsrc
     p <- linkGeoProgramWith [vs] gs [fs] m
     throwError
     (attrs,unis) <- getActives p
     return $ ShaderProgram (fromList attrs) (fromList unis) p

-- | Get all attributes and uniforms used by a program. Note that
-- unused parameters may be elided by the compiler, and so will not be
-- considered as active.
getActives :: Program -> 
              IO ( [(String, (AttribLocation, VariableType))]
                 , [(String, (UniformLocation, VariableType))] )
getActives p = 
  (,) <$> (get (activeAttribs p) >>= mapM (aux (attribLocation p)))
      <*> (get (activeUniforms p) >>= mapM (aux (uniformLocation p)))
  where aux f (_,t,name) = get (f name) >>= \l -> return (name, (l, t))

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
setUniform :: Uniform a => ShaderProgram -> String -> a -> IO ()
setUniform sp name = maybe (const (putStrLn warn >> return ()))
                           (\(u,_) -> let u' = uniform u
                                      in \x -> u' $= x)
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
