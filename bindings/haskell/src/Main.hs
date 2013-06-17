
module Main where

import Sound.Fae
import Foreign.C 

main = do
    putStrLn "This is Fae!"
    v <- c_VersionString
    v2 <- c_StringToUtf8 v
    v3 <- peekCString v2
    putStrLn $ v3