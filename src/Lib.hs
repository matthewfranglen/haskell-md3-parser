module Lib where

import Control.Lens ((%~), _1, Cons)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Bits ((.|.), shift, Bits)
import Data.Char (chr)
import Data.Int (Int16, Int32, Int64)
import Data.Word (Word8)

data MD3 = MD3 {
      header :: Header
    , frames :: [Frame]
    , tags :: [Tag]
    , surfaces :: [Surface]
    }

data Header = Header {
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

(==>) :: Monad m => (L.ByteString -> m (a, L.ByteString)) -> (a -> L.ByteString -> m (b, L.ByteString)) -> (L.ByteString -> m (b, L.ByteString))
(==>) f g bs = f bs >>= uncurry g

(=>>) :: Monad m => (L.ByteString -> m (a, L.ByteString)) -> (a -> b) -> (L.ByteString -> m b)
(=>>) f g bs = g' <$> f bs
    where g' = g . fst

-- Using _1 without importing it from Control.Lens will cause you to get errors about type holes. They are misleading!

takeAs :: (Word8 -> a) -> L.ByteString -> Maybe (a, L.ByteString)
takeAs f bs = (_1 %~ f) <$> L.uncons bs

takeIntegral :: Integral a => L.ByteString -> Maybe (a, L.ByteString)
takeIntegral = takeAs fromIntegral

takeIntegralAs :: Integral a => (a -> b) -> L.ByteString -> Maybe (b, L.ByteString)
takeIntegralAs f bs = (_1 %~ f . fromIntegral) <$> L.uncons bs

takeU8 :: L.ByteString -> Maybe (Char, L.ByteString)
takeU8 = takeIntegralAs chr

takeS16 :: L.ByteString -> Maybe (Int16, L.ByteString)
takeS16 = takeIntegral ==> \a -> takeIntegralAs $ toS16 a
    where toS16 :: Int16 -> Int16 -> Int16
          toS16 a b = (shift a 8) .|. b

takeS32 :: L.ByteString -> Maybe (Int32, L.ByteString)
takeS32 = takeIntegral ==>
    \a -> takeIntegral ==>
    \b -> takeIntegral ==>
    \c -> takeIntegralAs $ toS32 a b c
    where toS32 :: Int32 -> Int32 -> Int32 -> Int32 -> Int32
          toS32 a b c d = (shift a 24) .|. (shift b 16) .|. (shift c 8) .|. d
