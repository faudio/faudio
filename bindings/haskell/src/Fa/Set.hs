module Fa.Set where
import Foreign
import Foreign.C
import Fa
import Fa.List
import Fa.String
 
data Set_
 
type Set = Ptr Set_
 
foreign import ccall unsafe "fa_set_empty" empty :: IO Set
 
foreign import ccall unsafe "fa_set_single" single :: Ptr -> IO Set
 
foreign import ccall unsafe "fa_set_add" add ::
        Ptr -> Set -> IO Set
 
foreign import ccall unsafe "fa_set_set" set ::
        Ptr -> Set -> IO Set
 
foreign import ccall unsafe "fa_set_remove" remove ::
        Ptr -> Set -> IO Set
 
foreign import ccall unsafe "fa_set_dadd" dadd ::
        Ptr -> Set -> IO Set
 
foreign import ccall unsafe "fa_set_dset" dset ::
        Ptr -> Set -> IO Set
 
foreign import ccall unsafe "fa_set_dremove" dremove ::
        Ptr -> Set -> IO Set
 
foreign import ccall unsafe "fa_set_copy" copy :: Set -> IO Set
 
foreign import ccall unsafe "fa_set_destroy" destroy ::
        Set -> IO ()
 
foreign import ccall unsafe "fa_set_size" size :: Set -> IO CInt
 
foreign import ccall unsafe "fa_set_is_empty" isEmpty ::
        Set -> IO CInt
 
foreign import ccall unsafe "fa_set_is_single" isSingle ::
        Set -> IO CInt
 
foreign import ccall unsafe "fa_set_has" has ::
        Ptr -> Set -> IO CInt
 
foreign import ccall unsafe "fa_set_get" get ::
        Ptr -> Set -> IO Ptr
 
foreign import ccall unsafe "fa_set_is_subset_of" isSubsetOf ::
        Set -> Set -> IO CInt
 
foreign import ccall unsafe "fa_set_is_proper_subset_of"
        isProperSubsetOf :: Set -> Set -> IO CInt
 
foreign import ccall unsafe "fa_set_sum" sum ::
        Set -> Set -> IO Set
 
foreign import ccall unsafe "fa_set_intersection" intersection ::
        Set -> Set -> IO Set
 
foreign import ccall unsafe "fa_set_difference" difference ::
        Set -> Set -> IO Set
 
foreign import ccall unsafe "fa_set_product" product ::
        Set -> Set -> IO Set
 
foreign import ccall unsafe "fa_set_from_list" fromList ::
        List -> IO Set
 
foreign import ccall unsafe "fa_set_to_list" toList ::
        Set -> IO List

