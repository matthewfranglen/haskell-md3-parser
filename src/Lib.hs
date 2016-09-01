module Lib where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Bits ((.|.), shift)
import Data.Char (chr)
import Data.Int (Int16, Int64)
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

takeOne :: L.ByteString -> Maybe (Word8, L.ByteString)
takeOne = L.uncons

takeTwo :: L.ByteString -> Maybe ((Word8, Word8), L.ByteString)
takeTwo xs = if length x' == 2
             then Just ((x' !! 0, x' !! 1), xs')
             else Nothing
    where (x, xs') = L.splitAt 2 xs
          x'       = L.unpack x

takeFour :: L.ByteString -> Maybe ((Word8, Word8, Word8, Word8), L.ByteString)
takeFour xs = if length x' == 4
             then Just ((x' !! 0, x' !! 1, x' !! 2, x' !! 3), xs')
             else Nothing
    where (x, xs') = L.splitAt 4 xs
          x'       = L.unpack x

toU8 :: Word8 -> Char
toU8 = chr . fromIntegral

takeU8 :: L.ByteString -> Maybe (Char, L.ByteString)
takeU8 xs = f <$> takeOne xs
    where f (x', xs') = (toU8 x', xs')

toS16 :: (Word8, Word8) -> Int16
toS16 (x, y) = (shift x' 8) .|. y'
    where x' = fromIntegral x :: Int16
          y' = fromIntegral y :: Int16

takeS16 :: L.ByteString -> Maybe (Int16, L.ByteString)
takeS16 xs = f <$> takeTwo xs
    where f (x, xs') = (toS16 x, xs')
