-- | Simplify common drawing commands.
module Graphics.GLUtil.Drawing where
import Foreign.Ptr (nullPtr)
import Graphics.Rendering.OpenGL

-- | @drawIndexedTris n@ draws @n@ 'Triangles' using vertex data from
-- the currently bound 'ArrayBuffer' and indices from the beginning of
-- the currently bound 'ElementArrayBuffer'. Note that there must be
-- at least @n * 3@ indices in the 'ElementArrayBuffer'!
drawIndexedTris :: GLsizei -> IO ()
drawIndexedTris n = drawElements Triangles (n*3) UnsignedInt nullPtr