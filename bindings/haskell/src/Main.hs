
module Main where

import Sound.Ae
import Foreign.C 

main = do
    putStrLn "This is ae!"
    v <- c_VersionString
    v2 <- c_StringToUtf8 v
    v3 <- peekCString v2
    putStrLn $ v3