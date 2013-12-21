module Fa.Dynamic where
import Foreign
import Foreign.C
import Fa
import Fa.String
 
type TypeRepr = CInt
 
type Dynamic = Ptr ()
 
foreign import ccall unsafe "fa_dynamic_check" check ::
        Ptr -> IO CInt
 
foreign import ccall unsafe "fa_dynamic_get_type" getType ::
        Ptr -> IO TypeRepr

