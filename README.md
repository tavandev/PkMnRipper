# PokeRipper

https://github.com/magical/pokemon-sprites-rby/blob/master/rby-sprite-extract.go

https://bulbapedia.bulbagarden.net/wiki/Pokémon_base_stats_data_structure_in_Generation_I#Types

http://aurellem.org/vba-clojure/html/rom.html

{--
    spriteOffset :: (Ord a, Num a, Num b, Bits b) => a -> b -> b
    spriteOffset n b = shiftL (getBank n) 14 + b .&. 0x3FFF
        where 
        getBank a 
            | a < 0x1F  = 0x9
            | a < 0x4A  = 0xA
            | a < 0x74  = 0xB
            | a < 0x99  = 0xC
            | otherwise = 0xD 


    file = B.readFile "/Users/mimi/Desktop/pkmn.gbc"

    x s = pokemonBaseStats s & (!! 0) & ((!! 0xB) &&& (!! 0xC)) & process
        where 
        process (low, high) = 
            shiftL (fromIntegral high) 8 .|. fromIntegral low
--}