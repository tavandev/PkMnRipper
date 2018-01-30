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

data SpriteData = SpriteData
  { ptr    :: Word32
  , height :: Word8
  , width  :: Word8
  } deriving (Show)

spriteOffset :: Int -> B.ByteString -> SpriteData
spriteOffset idx s =
  let (w, h) = size idx s
  in SpriteData (shiftL (getBank idx) 14 + ptr idx s .&. 0x3FFF) w h
  where
    getBank a
        | a < 0x1F = 0x9
        | a < 0x4A = 0xA
        | a < 0x74 = 0xB
        | a < 0x99 = 0xC
        | otherwise = 0xD

    ptr n src =
      pokemonBaseStats src 
          & (!! n) 
          & ((!! 0xB) &&& (!! 0xC)) 
          & (\(low, high) -> shiftL (fromIntegral high) 8 .|. fromIntegral low)
    size n src =
      pokemonBaseStats src & (!! n) & (!! 0xA) &
      (\b -> (shiftR (fromIntegral b) 4 * 8, fromIntegral b .&. 0xF * 8))

hex n = "0x" ++ showHex n ""
