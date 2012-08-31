{-# LANGUAGE DeriveDataTypeable #-}
-- |Miscellaneous utilities for dealing with OpenGL errors.
module Graphics.GLUtil.GLError (printError, printErrorMsg, throwError, 
                                GLError, throwErrorMsg) where
import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Graphics.Rendering.OpenGL
import System.IO (hPutStrLn, stderr)

-- |Check OpenGL error flags and print them on 'stderr'.
printError :: IO ()
printError = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)

-- |Check OpenGL error flags and print them on 'stderr' with the given
-- message as a prefix. If there are no errors, nothing is printed.
printErrorMsg :: String -> IO ()
printErrorMsg msg = do errs <- get errors
                       when (not (null errs))
                            (putStrLn msg >> mapM_ printErr errs)
  where printErr = hPutStrLn stderr . ("  GL: "++) . show

-- |An exception type for OpenGL errors.
data GLError = GLError String deriving (Typeable)
instance Exception GLError where
instance Show GLError where
  show (GLError msg) = "GLError " ++ msg

-- |Prefix each of a list of messages with "GL: ".
printGLErrors :: Show a => [a] -> String
printGLErrors = intercalate "\n  GL: " . ("" :) . map show

-- |Throw an exception if there is an OpenGL error.
throwError :: IO ()
throwError = do errs <- get errors
                when (not (null errs))
                     (throwIO . GLError . tail $ printGLErrors errs)

-- |Throw an exception if there is an OpenGL error. The exception's
-- error message is prefixed with the supplied 'String'.
throwErrorMsg :: String -> IO ()
throwErrorMsg msg = do errs <- get errors
                       when (not (null errs))
                            (throwIO $ GLError (msg++printGLErrors errs))
