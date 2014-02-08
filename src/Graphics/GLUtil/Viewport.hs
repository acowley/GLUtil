-- | Helpers for working with OpenGL viewports.
module Graphics.GLUtil.Viewport where
import Control.Monad.IO.Class
import Graphics.Rendering.OpenGL

-- | @withViewport pos sz m@ runs the action @m@ after setting the
-- viewport with the given 'Position' and 'Size'. The viewport is
-- reset to its original state after the action is run, and the result
-- of the action is returned.
withViewport :: MonadIO m => Position -> Size -> m a -> m a
withViewport p s m = do oldVP <- liftIO $ do oldVP <- get viewport
                                             viewport $= (p,s)
                                             return oldVP
                        r <- m
                        liftIO $ viewport $= oldVP
                        return r
