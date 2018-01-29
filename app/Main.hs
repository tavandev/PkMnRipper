module Main where

import           Data.Aeson
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BS
import           Lib

main :: IO ()
main = do
  e <- B.readFile "/Users/mimi/Desktop/pkmn.gbc"
  BS.writeFile "/Users/mimi/Desktop/pkmn.json" $ encode $ toPokemons e
