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


takeIntegral :: Integral a => Bits a => L.ByteString -> Maybe (a, L.ByteString)
takeIntegral bs = (_1 %~ fromIntegral) <$> L.uncons bs

takeU8 :: L.ByteString -> Maybe (Char, L.ByteString)
takeU8 bs = (_1 %~ chr) <$> takeIntegral bs

takeS16 :: L.ByteString -> Maybe (Int16, L.ByteString)
takeS16 = takeIntegral ==> \a -> takeIntegral ==> \b -> return . (,) (toS16 a b)
    where toS16 :: Int16 -> Int16 -> Int16
          toS16 a b = (shift a 8) .|. b


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
