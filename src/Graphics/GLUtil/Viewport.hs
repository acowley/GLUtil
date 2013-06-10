module Graphics.GLUtil.Viewport where
import Graphics.Rendering.OpenGL

withViewport :: Position -> Size -> IO a -> IO a
withViewport p s m = do oldVP <- get viewport
                        viewport $= (p,s)
                        r <- m
                        viewport $= oldVP
                        return r
