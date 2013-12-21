module Fa.List where
import Foreign
import Foreign.C
import Fa
 
data List_
 
type List = Ptr List_
 
foreign import ccall unsafe "fa_list_empty" empty :: IO List
 
foreign import ccall unsafe "fa_list_single" single ::
        FaPtr -> IO List
 
foreign import ccall unsafe "fa_list_cons" cons ::
        FaPtr -> List -> IO List
 
foreign import ccall unsafe "fa_list_dcons" dcons ::
        FaPtr -> List -> IO List
 
foreign import ccall unsafe "fa_list_repeat" repeat ::
        CInt -> FaPtr -> IO List
 
foreign import ccall unsafe "fa_list_enumerate" enumerate ::
        CInt -> CInt -> IO List
 
foreign import ccall unsafe "fa_list_copy" copy :: List -> IO List
 
foreign import ccall unsafe "fa_list_destroy" destroy ::
        List -> IO ()
 
foreign import ccall unsafe "fa_list_is_empty" isEmpty ::
        List -> IO CInt
 
foreign import ccall unsafe "fa_list_is_single" isSingle ::
        List -> IO CInt
 
foreign import ccall unsafe "fa_list_length" length ::
        List -> IO CInt
 
foreign import ccall unsafe "fa_list_head" head :: List -> IO FaPtr
 
foreign import ccall unsafe "fa_list_tail" tail :: List -> IO List
 
foreign import ccall unsafe "fa_list_dtail" dtail ::
        List -> IO List
 
foreign import ccall unsafe "fa_list_init" init :: List -> IO List
 
foreign import ccall unsafe "fa_list_dinit" dinit ::
        List -> IO List
 
foreign import ccall unsafe "fa_list_last" last :: List -> IO FaPtr
 
foreign import ccall unsafe "fa_list_append" append ::
        List -> List -> IO List
 
foreign import ccall unsafe "fa_list_dappend" dappend ::
        List -> List -> IO List
 
foreign import ccall unsafe "fa_list_reverse" reverse ::
        List -> IO List
 
foreign import ccall unsafe "fa_list_dreverse" dreverse ::
        List -> IO List
 
foreign import ccall unsafe "fa_list_sort" sort :: List -> IO List
 
foreign import ccall unsafe "fa_list_dsort" dsort ::
        List -> IO List
 
foreign import ccall unsafe "fa_list_take" take ::
        CInt -> List -> IO List
 
foreign import ccall unsafe "fa_list_dtake" dtake ::
        CInt -> List -> IO List
 
foreign import ccall unsafe "fa_list_drop" drop ::
        CInt -> List -> IO List
 
foreign import ccall unsafe "fa_list_ddrop" ddrop ::
        CInt -> List -> IO List
 
foreign import ccall unsafe "fa_list_index" index ::
        CInt -> List -> IO FaPtr
 
foreign import ccall unsafe "fa_list_range" range ::
        CInt -> CInt -> List -> IO List
 
foreign import ccall unsafe "fa_list_insert" insert ::
        CInt -> FaPtr -> List -> IO List
 
foreign import ccall unsafe "fa_list_dinsert" dinsert ::
        CInt -> FaPtr -> List -> IO List
 
foreign import ccall unsafe "fa_list_insert_range" insertRange ::
        CInt -> List -> List -> IO List
 
foreign import ccall unsafe "fa_list_dinsert_range" dinsertRange ::
        CInt -> List -> List -> IO List
 
foreign import ccall unsafe "fa_list_remove" remove ::
        CInt -> List -> IO List
 
foreign import ccall unsafe "fa_list_dremove" dremove ::
        CInt -> List -> IO List
 
foreign import ccall unsafe "fa_list_remove_range" removeRange ::
        CInt -> CInt -> List -> IO List
 
foreign import ccall unsafe "fa_list_dremove_range" dremoveRange ::
        CInt -> CInt -> List -> IO List
 
foreign import ccall unsafe "fa_list_has" has ::
        FaPtr -> List -> IO CInt
 
foreign import ccall unsafe "fa_list_find" find ::
        Pred -> FaPtr -> List -> IO FaPtr
 
foreign import ccall unsafe "fa_list_index_of" indexOf ::
        FaPtr -> List -> IO CInt
 
foreign import ccall unsafe "fa_list_find_index" findIndex ::
        Pred -> FaPtr -> List -> IO CInt
 
foreign import ccall unsafe "fa_list_filter" filter ::
        Pred -> FaPtr -> List -> IO List
 
foreign import ccall unsafe "fa_list_dfilter" dfilter ::
        Pred -> FaPtr -> List -> IO List
 
foreign import ccall unsafe "fa_list_map" map ::
        Unary -> FaPtr -> List -> IO List
 
foreign import ccall unsafe "fa_list_dmap" dmap ::
        Unary -> FaPtr -> List -> IO List
 
foreign import ccall unsafe "fa_list_join_map" joinMap ::
        Unary -> FaPtr -> List -> IO List
 
foreign import ccall unsafe "fa_list_djoin_map" djoinMap ::
        Unary -> FaPtr -> List -> IO List
 
foreign import ccall unsafe "fa_list_join" join :: List -> IO List
 
foreign import ccall unsafe "fa_list_djoin" djoin ::
        List -> IO List
 
foreign import ccall unsafe "fa_list_fold_left" foldLeft ::
        Binary -> FaPtr -> FaPtr -> List -> IO FaPtr
 
foreign import ccall unsafe "fa_list_dfold_left" dfoldLeft ::
        Binary -> FaPtr -> FaPtr -> List -> IO FaPtr
 
foreign import ccall unsafe "fa_list_to_list" toList ::
        List -> IO List

