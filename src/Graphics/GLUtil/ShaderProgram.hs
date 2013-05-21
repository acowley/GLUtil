-- |Convenience interface for working with GLSL shader
-- programs. Provides an interface for setting attributes and
-- uniforms.
module Graphics.GLUtil.ShaderProgram (ShaderProgram(..), loadShaderProgram, 
                                      loadShaderProgramWith,
                                      loadShaderExplicit, getAttrib,
                                      enableAttrib, setAttrib, 
                                      setUniform, getUniform) where
import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Data.List (find, findIndex)
import Data.Map.Strict (Map, fromList, lookup)
import Data.Maybe (isJust, isNothing, catMaybes)
import Graphics.GLUtil.Shaders (loadShader, linkShaderProgram,
                                linkShaderProgramWith)
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

-- |Load a 'ShaderProgram' from a vertex shader source file and a
-- fragment shader source file. The active attributes and uniforms in
-- the linked program are recorded in the 'ShaderProgram'. The
-- supplied 'IO' function is applied to the new program after shader
-- objects are attached to the program, but before linking. This
-- supports the use of 'bindFragDataLocation' to map fragment shader
-- outputs.
loadShaderProgramWith :: FilePath -> FilePath -> (Program -> IO ())
                      -> IO ShaderProgram
loadShaderProgramWith vsrc fsrc m = 
  do vs <- loadShader vsrc
     fs <- loadShader fsrc
     p <- linkShaderProgramWith [vs] [fs] m
     throwError
     (attrs,unis) <- getActives p
     return $ ShaderProgram (fromList attrs) (fromList unis) p

getActives :: Program -> 
              IO ( [(String, (AttribLocation, VariableType))]
                 , [(String, (UniformLocation, VariableType))] )
getActives p = 
  (,) <$> (get (activeAttribs p) >>= mapM (aux (attribLocation p)))
      <*> (get (activeUniforms p) >>= mapM (aux (uniformLocation p)))
  where aux f (_,t,name) = get (f name) >>= \l -> return (name, (l, t))

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

setUniform :: Uniform a => ShaderProgram -> String -> a -> IO ()
setUniform sp name = maybe (const (putStrLn warn >> return ()))
                           (\(u,_) -> let u' = uniform u
                                      in \x -> u' $= x)
                           (lookup name $ uniforms sp)
  where warn = "WARNING: uniform "++name++" is not active"

getUniform :: ShaderProgram -> String -> UniformLocation
getUniform sp n = maybe (error msg) fst . lookup n $ uniforms sp
  where msg = "Uniform "++show n++" is not active"

setAttrib :: ShaderProgram -> String -> 
             IntegerHandling -> VertexArrayDescriptor a -> IO ()
setAttrib sp name = maybe (\_ _ -> putStrLn warn >> return ())
                          (\(a,_) -> let vap = vertexAttribPointer a
                                     in \ih vad -> (($= (ih, vad)) vap))
                          (lookup name $ attribs sp)
  where warn = "WARNING: attrib "++name++" is not active"

getAttrib :: ShaderProgram -> String -> AttribLocation
getAttrib sp n = maybe (error msg) fst . lookup n $ attribs sp
  where msg = "Attrib "++show n++" is not active"

enableAttrib :: ShaderProgram -> String -> IO ()
enableAttrib sp name = maybe (return ())
                             (($= Enabled) . vertexAttribArray . fst)
                             (lookup name $ attribs sp)
