module Fa.Map where
import Foreign
import Foreign.C
import Fa
import Fa.Pair
import Fa.List
import Fa.String
 
data Map_
 
type Map = Ptr Map_
 
type Key = Ptr
 
foreign import ccall unsafe "fa_map_empty" empty :: IO Map
 
foreign import ccall unsafe "fa_map_copy" copy :: Map -> IO Map
 
foreign import ccall unsafe "fa_map_destroy" destroy ::
        Map -> IO ()
 
foreign import ccall unsafe "fa_map_size" size :: Map -> IO CInt
 
foreign import ccall unsafe "fa_map_is_empty" isEmpty ::
        Map -> IO CInt
 
foreign import ccall unsafe "fa_map_is_single" isSingle ::
        Map -> IO CInt
 
foreign import ccall unsafe "fa_map_add" add ::
        Key -> Ptr -> Map -> IO Map
 
foreign import ccall unsafe "fa_map_set" set ::
        Key -> Ptr -> Map -> IO Map
 
foreign import ccall unsafe "fa_map_remove" remove ::
        Key -> Map -> IO Map
 
foreign import ccall unsafe "fa_map_dadd" dadd ::
        Key -> Ptr -> Map -> IO Map
 
foreign import ccall unsafe "fa_map_dset" dset ::
        Key -> Ptr -> Map -> IO Map
 
foreign import ccall unsafe "fa_map_dremove" dremove ::
        Key -> Map -> IO Map
 
foreign import ccall unsafe "fa_map_add_entry" addEntry ::
        Pair -> Map -> IO Map
 
foreign import ccall unsafe "fa_map_set_entry" setEntry ::
        Pair -> Map -> IO Map
 
foreign import ccall unsafe "fa_map_remove_entry" removeEntry ::
        Pair -> Map -> IO Map
 
foreign import ccall unsafe "fa_map_get" get ::
        Key -> Map -> IO Ptr
 
foreign import ccall unsafe "fa_map_has_key" hasKey ::
        Key -> Map -> IO CInt
 
foreign import ccall unsafe "fa_map_has_elem" hasElem ::
        Ptr -> Map -> IO CInt
 
foreign import ccall unsafe "fa_map_has_entry" hasEntry ::
        Pair -> Map -> IO CInt
 
foreign import ccall unsafe "fa_map_is_submap_of" isSubmapOf ::
        Map -> Map -> IO CInt
 
foreign import ccall unsafe "fa_map_is_proper_submap_of"
        isProperSubmapOf :: Map -> Map -> IO CInt
 
foreign import ccall unsafe "fa_map_sum" sum ::
        Map -> Map -> IO Map
 
foreign import ccall unsafe "fa_map_product" product ::
        Map -> Map -> IO Map
 
foreign import ccall unsafe "fa_map_difference" difference ::
        Map -> Map -> IO Map
 
foreign import ccall unsafe "fa_map_map" map ::
        Unary -> Ptr -> Map -> IO Map
 
foreign import ccall unsafe "fa_map_from_pair" fromPair ::
        Pair -> IO Map
 
foreign import ccall unsafe "fa_map_from_list" fromList ::
        List -> IO Map
 
foreign import ccall unsafe "fa_map_to_list" toList ::
        Map -> IO List

