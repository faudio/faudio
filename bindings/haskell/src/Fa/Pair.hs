module Fa.Pair where
import Foreign
import Foreign.C
import Fa
import Fa.List
 
data Pair_
 
type Pair = Ptr Pair_
 
type Struct = Ptr ()
 
foreign import ccall unsafe "fa_pair_create" create ::
        FaPtr -> FaPtr -> IO Pair
 
-- foreign import ccall unsafe "fa_pair_read" read ::
        -- FaPtr Struct -> IO Pair
 
-- foreign import ccall unsafe "fa_pair_write" write ::
        -- FaPtr Struct -> Pair -> IO ()
 
foreign import ccall unsafe "fa_pair_copy" copy :: Pair -> IO Pair
 
foreign import ccall unsafe "fa_pair_destroy" destroy ::
        Pair -> IO ()
 
-- foreign import ccall unsafe "fa_pair_decons" decons ::
        -- FaPtr FaPtr -> FaPtr FaPtr -> Pair -> IO ()
 
foreign import ccall unsafe "fa_pair_first" first :: Pair -> IO FaPtr
 
foreign import ccall unsafe "fa_pair_second" second ::
        Pair -> IO FaPtr
 
foreign import ccall unsafe "fa_pair_duplicate" duplicate ::
        FaPtr -> IO Pair
 
foreign import ccall unsafe "fa_pair_swap" swap :: Pair -> IO Pair
 
foreign import ccall unsafe "fa_pair_assoc" assoc ::
        Pair -> IO Pair
 
foreign import ccall unsafe "fa_pair_unassoc" unassoc ::
        Pair -> IO Pair
 
foreign import ccall unsafe "fa_pair_to_list" toList ::
        Pair -> IO List

