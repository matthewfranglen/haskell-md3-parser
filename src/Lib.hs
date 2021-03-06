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
      version       :: Int32
    , name          :: String
    , flags         :: Int32
    , frameCount    :: Int32
    , tagCount      :: Int32
    , surfaceCount  :: Int32
    , skinCount     :: Int32
    , frameOffset   :: Int32
    , surfaceOffset :: Int32
    , size          :: Int32
}

data Frame = Frame {
    }

data Tag = Tag {
    }

data Surface = Surface {
    }

data ParseState = ParseState {
      content :: L.ByteString
    }
newtype Parse a = Parse {
      runParse :: ParseState -> Maybe (a, ParseState)
    }
instance Functor Parse where
    fmap f parser = parser ==> \result ->
                    identity (f result)

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

(==>) :: Parse a -> (a -> Parse b) -> Parse b
(==>) x f = snd <$> x >>= f

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

takeMany :: Integral a => a -> (L.ByteString -> Maybe (b, L.ByteString)) -> L.ByteString -> Maybe ([b], L.ByteString)
takeMany n f bs | n <= 0    = Just ([], bs)
                | n == 1    = (_1 %~ (:[])) <$> f bs
                | otherwise = f bs >>= \(a, bs') -> (_1 %~ (a :)) <$> takeMany (n - 1) f bs'

-- takeMD3Header :: L.ByteString -> Maybe Header
-- takeMD3Header = takeS32 ==>
--     \ident -> takeS32 ==>
--     \version -> takeMany 64 takeU8 ==>
--     \name -> takeS32 ==>
--     \flags -> takeS32 ==>
--     \frameCount -> takeS32 ==>
--     \tagCount -> takeS32 ==>
--     \surfaceCount -> takeS32 ==>
--     \skinCount -> takeS32 ==>
--     \frameOffset -> takeS32 ==>
--     \tagOffset -> takeS32 ==>
--     \surfaceOffset -> takeS32 ==>
--     \skinOffset -> takeS32 =>>
--     \size -> Header {
--       version = version
--     , name = name
--     , flags = flags
--     , frameCount = frameCount
--     , tagCount = tagCount
--     , surfaceCount = surfaceCount
--     , skinCount = skinCount
--     , frameOffset = frameOffset
--     , surfaceOffset = surfaceOffset
--     , size = size
--     }
