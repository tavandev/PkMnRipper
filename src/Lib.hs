{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
  ( Pokemon
  , pokemonBaseStats
  , indexOrder
  , types
  , toPokemons
  ) where

import           Control.Arrow
import           Data.Aeson
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Function
import           Data.List
import           Data.Monoid
import           Data.Word
import           Numeric
import           Utils

data Pokemon = Pokemon
  { index      :: Word8
  , internalID :: Word8
  , name       :: String
  , hp         :: Word8
  , attack     :: Word8
  , defense    :: Word8
  , speed      :: Word8
  , special    :: Word8
  , type1      :: String
  , type2      :: Maybe String
  } deriving (Show)

instance ToJSON Pokemon where
  toJSON Pokemon {..} =
    object
      [ "index" .= index
      , "name" .= name
      , "hp" .= hp
      , "attack" .= attack
      , "defense" .= defense
      , "speed" .= speed
      , "special" .= special
      , "type1" .= type1
      , "type2" .= type2
      ]
  toEncoding Pokemon {..} =
    pairs $
    "index" .= index <> "name" .= name <> "hp" .= hp <> "attack" .= attack <>
    "defense" .=
    defense <>
    "speed" .=
    speed <>
    "special" .=
    special <>
    "type1" .=
    type1 <>
    "type2" .=
    type2

indexOrder :: B.ByteString -> [Word8]
indexOrder = slice 0x41036 190 >>> B.unpack

pokemonBaseStats :: B.ByteString -> [[Word8]]
pokemonBaseStats = slice 0x0383DE 4228 >>> B.unpack >>> chunks 28

types :: B.ByteString -> [String]
types s = toTypeArray s <$> typePointers s
  where
    typePointers =
      slice 0x27D63 0x36 >>> B.unpack >>> chunks 2 >>> fmap processP
    processP (low:high:_) =
      shiftL 2 16 .|. shiftL (fromIntegral high) 8 .|. fromIntegral low
    toTypeArray source offset =
      B.splitAt offset source & \(_, d) ->
        B.takeWhile (/= 0x50) d & B.unpack & fmap toChar & capitalized

toPokemons :: B.ByteString -> [Pokemon]
toPokemons s = toPkmn s <$> zip (sortedPokemon s) (pokemonBaseStats s)
  where
    doubleType a b
      | a == b = (a, Nothing)
      | otherwise = (a, Just b)
    pokemonNames src =
      slice 0xE8000 1900 src & B.unpack & fmap toChar & chunks 10 &
      fmap (filter (/= ' ') >>> capitalized)
    sortedPokemon src =
      zip3 (indexOrder src) [1 ..] (pokemonNames src) &
      filter (\(idx, _, _) -> idx /= 0) &
      sort
    toPkmn src ((idx, int, name), b) =
      let (t1, t2) =
            doubleType
              (fromIntegral (b !! 6) & (types src !!))
              (fromIntegral (b !! 7) & (types src !!))
      in Pokemon idx int name (b !! 1) (b !! 2) (b !! 3) (b !! 4) (b !! 5) t1 t2