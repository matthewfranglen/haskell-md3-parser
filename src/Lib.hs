module Lib where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (chr)
import Data.Int (Int64)
import Data.Word (Word8)

data MD3 = MD3 {
      version :: Int
    , name :: String
    , flags :: Int
    , frameCount :: Int
    , tagCount :: Int
    , surfaceCount :: Int
    , skinCount :: Int
    , frameOffset :: Int
    , surfaceOffset :: Int
    , size :: Int
    , frames :: [Frame]
    , tags :: [Tag]
    , surfaces :: [Surface]
    }

data Frame = Frame {
    }

data Tag = Tag {
    }

data Surface = Surface {
    }

data ParseState = ParseState {
      content :: L.ByteString
    , offset :: Int64
    }

toU8 :: Word8 -> Char
toU8 = chr . fromIntegral

takeU8 :: L.ByteString -> Maybe (Char, L.ByteString)
takeU8 xs = f <$> L.uncons xs
    where f (x', xs') = (toU8 x', xs')
