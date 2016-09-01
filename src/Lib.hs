module Lib
    ( parse
    ) where

data MD3 {
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

data Frame {
    }

data Tag {
    }

data Surface {
    }

parse = undefined
