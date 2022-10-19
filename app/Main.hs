{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts    #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as CH8
import Data.ByteString.Builder as BLD
import Data.Binary.Builder as BIN
import Data.Word
import Data.Int
import Numeric (readHex)
import Data.Bits as BIT
import Data.Bits.ByteString as BIT

import Data.Digest.CRC as CRC
import Data.Digest.CRC8 as CRC

class HexBuilder a where
  buildHex :: a -> Builder

instance HexBuilder Word8 where
  buildHex = word8Hex

-- | Show a bytestring as hex
prettyPrint :: BS.ByteString -> BS.ByteString
prettyPrint = builderToBS . BLD.byteStringHex

-- | Convert a builder type to a bytestring
builderToBS :: BLD.Builder -> BS.ByteString
builderToBS = BL.toStrict . BLD.toLazyByteString

-- | Convert Int16 to their big endian two byte representation
intLabel :: Int16 -> BS.ByteString
intLabel = builderToBS . BLD.int16BE

-- | returns in hex
checksum :: BS.ByteString -> BS.ByteString
checksum m = (builderToBS . buildHex . crcWord) (CRC.digest m :: CRC8)

-- | Get the asset label for a given Int16 as a bytestring.
getLabel :: Int16 -> BS.ByteString
getLabel n = let label = BLD.int16BE $ fromIntegral n
                 checksumLabel = (Prelude.fst . Prelude.head . readHex . CH8.unpack . checksum  . builderToBS) label
                 checkSum = BLD.int8 $ fromIntegral checksumLabel
             in BIT.shift (builderToBS $ label <> checkSum <> BLD.int8 0) (-4)

main :: IO ()
main = do line <- Prelude.getLine
          let int16 = read line :: Int16
          CH8.putStrLn . prettyPrint $ getLabel int16
