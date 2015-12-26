module EasyLoader(EasyData(..), loadEasyVector) where

import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.Vector.Storable as V

import Data.Vector.Storable.MMap

import           Unsafe.Coerce

import           Debug.Trace

data EasyData = EasyData (V.Vector Float) (V.Vector Int)

getVectorSizes :: Get (Int, Int)
getVectorSizes = liftA2 (,) (fromIntegral <$> getWord32le) (fromIntegral <$> getWord32le)


readVectors :: FilePath -> (Int, Int) -> IO (V.Vector Float, V.Vector Int)
readVectors path (vertSize, indsSize) = do
  b <- BS.readFile path
  let (fptr, offset, len) = BS.toForeignPtr b
  return (V.unsafeCast (V.unsafeFromForeignPtr fptr 8 (vertSize * 4 * 16)) :: V.Vector Float,
          V.unsafeCast (V.unsafeFromForeignPtr fptr (vertSize * 4 * 16 + 8 + 4) (indsSize * 4)) :: V.Vector Int)

loadEasyVector :: FilePath -> IO (V.Vector Float, V.Vector Int)
loadEasyVector path = B.readFile path >>= return . runGet getVectorSizes >>= readVectors path
