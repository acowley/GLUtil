{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.GLUtil.Misc (printError, printErrorMsg, throwError, 
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

data GLError = GLError String deriving (Typeable)
instance Exception GLError where
instance Show GLError where
  show (GLError msg) = "GLError " ++ msg

printGLErrors :: Show a => [a] -> String
printGLErrors = intercalate "\n  GL: " . ("" :) . map show

throwError :: IO ()
throwError = do errs <- get errors
                when (not (null errs))
                     (throwIO . GLError . tail $ printGLErrors errs)

throwErrorMsg :: String -> IO ()
throwErrorMsg msg = do errs <- get errors
                       when (not (null errs))
                            (throwIO $ GLError (msg++printGLErrors errs))
