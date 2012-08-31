-- Working with textures will require loading images from disk. We will
-- use the TGA format since it is quite simple to work with.
module TGA (readTGA) where
import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

-- |The header structure contains quite a bit of information, but we are
-- only concerned with whether the image is RGB color or grayscale, and
-- what its dimensions are.
readImage :: Get (Int, Int, BS.ByteString)
readImage = do skip 2
               isColor <- (==2) <$> getWord8
               skip 9
               width <- fromIntegral <$> getWord16le
               height <- fromIntegral <$> getWord16le
               _bpp <- fromIntegral <$> getWord8 :: Get Int
               skip 1
               let bytesPerPixel = if isColor then 3 else 1
               pixels <- getByteString (width*height*bytesPerPixel)
               return (width, height, pixels)

-- |Read a TGA image from a file.
readTGA :: FilePath -> IO (Int, Int, BS.ByteString)
readTGA = return . runGet readImage <=< BL.readFile
