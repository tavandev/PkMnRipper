module Utils
  ( slice
  , chunks
  , capitalized
  , characterEncoding
  , toChar
  ) where

import qualified Data.ByteString as B
import           Data.Char
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe
import           Data.Word

slice :: Int -> Int -> B.ByteString -> B.ByteString
slice o l s =
  let (_, b) = B.splitAt o s
  in B.take l b

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
  in ys : chunks n zs

capitalized :: String -> String
capitalized []     = []
capitalized (x:xs) = toUpper x : (toLower <$> xs)

characterEncoding :: Map Word8 Char
characterEncoding =
  M.fromList
    [ (128, 'A')
    , (129, 'B')
    , (130, 'C')
    , (131, 'D')
    , (132, 'E')
    , (133, 'F')
    , (134, 'G')
    , (135, 'H')
    , (136, 'I')
    , (137, 'J')
    , (138, 'K')
    , (139, 'L')
    , (140, 'M')
    , (141, 'N')
    , (142, 'O')
    , (143, 'P')
    , (144, 'Q')
    , (145, 'R')
    , (146, 'S')
    , (147, 'T')
    , (148, 'U')
    , (149, 'V')
    , (150, 'W')
    , (151, 'X')
    , (152, 'Y')
    , (153, 'Z')
    , (232, '.')
    , (239, '\9794')
    , (245, '\9792')
    ]

toChar :: Word8 -> Char
toChar a = fromMaybe ' ' $ M.lookup a characterEncoding
