module Graphics.GLUtil.Misc where
import Control.Monad (when)
import Graphics.Rendering.OpenGL
import System.IO (hPutStrLn, stderr)

-- |Check OpenGL error flags and print them on 'stderr'.
printErrors :: IO ()
printErrors = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)

-- |Check OpenGL error flags and print them on 'stderr' with the given
-- message as a prefix. If there are no errors, nothing is printed.
printErrorsMsg :: String -> IO ()
printErrorsMsg msg = do errs <- get errors
                        when (not (null errs))
                             (putStrLn msg >> mapM_ printErr errs)
  where printErr = hPutStrLn stderr . ("  GL: "++) . show
