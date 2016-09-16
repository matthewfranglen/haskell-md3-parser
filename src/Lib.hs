module Lib where

import Control.Lens ((%~), _1, Cons)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Bits ((.|.), shift, Bits)
import Data.Char (chr)
import Data.Int (Int16, Int32, Int64)
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


takeU8 :: L.ByteString -> Maybe (Char, L.ByteString)
takeU8 xs = (_1 %~ toU8) <$> L.uncons xs

toU8 :: Word8 -> Char
toU8 = chr . fromIntegral


takeS16 :: L.ByteString -> Maybe (Int16, L.ByteString)
takeS16 xs = Just (0, xs) >>= f >>= f
    where f :: (Int16, L.ByteString) -> Maybe (Int16, L.ByteString)
          f = uncurry appendConsumedBytes

takeS32 :: L.ByteString -> Maybe (Int32, L.ByteString)
takeS32 xs = Just (0, xs) >>= f >>= f >>= f >>= f
    where f :: (Int32, L.ByteString) -> Maybe (Int32, L.ByteString)
          f = uncurry appendConsumedBytes

appendBytes :: Integral a => Integral b => Bits b => b -> a -> b
appendBytes a b = (shift a 8) .|. fromIntegral b

-- Using _1 without importing it from Control.Lens will cause you to get errors about type holes. They are misleading!

consumeBytes :: (Word8 -> a) -> L.ByteString -> Maybe (a, L.ByteString)
consumeBytes f xs = (_1 %~ f) <$> L.uncons xs

appendConsumedBytes :: Integral a => Bits a => a -> L.ByteString -> Maybe (a, L.ByteString)
appendConsumedBytes v xs = consumeBytes (appendBytes v) xs
