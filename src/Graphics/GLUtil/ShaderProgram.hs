-- |Convenience interface for working with GLSL shader
-- programs. Provides an interface for setting attributes and
-- uniforms.
module Graphics.GLUtil.ShaderProgram (ShaderProgram(..), loadShaderProgram, 
                                      loadShaderExplicit, getAttrib,
                                      enableAttrib, setAttrib, 
                                      setUniform, getUniform) where
import Prelude hiding (lookup)
import Control.Applicative (pure, (<$>), (<*>))
import Data.Map.Strict (Map, fromList, lookup)
import Graphics.GLUtil.Shaders (loadShader, linkShaderProgram)
import Graphics.GLUtil.GLError (throwError)
import Graphics.Rendering.OpenGL

-- |Representation of a GLSL shader program that has been compiled and
-- linked.
data ShaderProgram = ShaderProgram { attribs  :: Map String AttribLocation
                                   , uniforms :: Map String UniformLocation
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
-- fragment shader source file. If the third argument is @Nothing@,
-- then only the active attributes and uniforms in the linked program
-- are recorded in the 'ShaderProgram'. If the third argument is @Just
-- (attribNames, uniformNames)@, then the locations associated with
-- this names are all queried and recorded in the 'ShaderProgram'.
loadShaderProgram :: FilePath -> FilePath -> IO ShaderProgram
loadShaderProgram vsrc fsrc = 
  do vs <- loadShader vsrc
     fs <- loadShader fsrc
     p <- linkShaderProgram [vs] [fs]
     throwError
     (attrs,unis) <- getActives p
     return $ ShaderProgram (fromList attrs) (fromList unis) p

getActives :: Program -> 
              IO ([(String,AttribLocation)], [(String,UniformLocation)])
getActives p = (,) <$> (get (activeAttribs p) >>= mapM (aux (attribLocation p)))
                   <*> (get (activeUniforms p) >>= mapM (aux (uniformLocation p)))
  where aux f (_,_,name) = (,) <$> pure name <*> get (f name)

getExplicits :: Program -> ([String], [String]) ->
                IO ([(String,AttribLocation)], [(String,UniformLocation)])
getExplicits p (anames,unames) = (,) <$> mapM (aux (attribLocation p)) anames
                                     <*> mapM (aux (uniformLocation p)) unames
  where aux f name = (,) <$> pure name <*> get (f name)

setUniform :: Uniform a => ShaderProgram -> String -> a -> IO ()
setUniform sp name = maybe (const (putStrLn warn >> return ()))
                           (\u -> let u' = uniform u
                                  in \x -> u' $= x)
                           (lookup name $ uniforms sp)
  where warn = "WARNING: uniform "++name++" is not active"

getUniform :: ShaderProgram -> String -> UniformLocation
getUniform sp n = maybe (error msg) id . lookup n $ uniforms sp
  where msg = "Uniform "++show n++" is not active"

setAttrib :: ShaderProgram -> String -> 
             IntegerHandling -> VertexArrayDescriptor a -> IO ()
setAttrib sp name = maybe (\_ _ -> putStrLn warn >> return ())
                          (\a -> let vap = vertexAttribPointer a
                                 in \ih vad -> (($= (ih, vad)) vap))
                          (lookup name $ attribs sp)
  where warn = "WARNING: attrib "++name++" is not active"

getAttrib :: ShaderProgram -> String -> AttribLocation
getAttrib sp n = maybe (error msg) id . lookup n $ attribs sp
  where msg = "Attrib "++show n++" is not active"

enableAttrib :: ShaderProgram -> String -> IO ()
enableAttrib sp name = maybe (return ())
                             (($= Enabled) . vertexAttribArray)
                             (lookup name $ attribs sp)
