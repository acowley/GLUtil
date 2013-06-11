-- | Helpers for working with OpenGL viewports.
module Graphics.GLUtil.Viewport where
import Graphics.Rendering.OpenGL

-- | @withViewport pos sz m@ runs the action @m@ after setting the
-- viewport with the given 'Position' and 'Size'. The viewport is
-- reset to its original state after the action is run, and the result
-- of the action is returned.
withViewport :: Position -> Size -> IO a -> IO a
withViewport p s m = do oldVP <- get viewport
                        viewport $= (p,s)
                        r <- m
                        viewport $= oldVP
                        return r
