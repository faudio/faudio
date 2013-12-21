
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Foreign.C 
import Foreign.Ptr
import Fa
import Fa.Fa
import Fa.List
import Fa.Signal
import Control.Concurrent (threadDelay)
import qualified Fa.List as List
import qualified Fa.Signal as Signal
import Fa.String (FaString, toUtf8, fromUtf8)
import qualified Fa.Audio as Audio
import System.IO.Unsafe (unsafePerformIO)

-- openStream i o p =

type Proc = FaPtr -> List -> List  
foreign import ccall "wrapper" mkProc :: Proc -> IO (FunPtr Proc)

openStream' :: Audio.Device -> Audio.Device -> ([Signal] -> [Signal]) -> IO Audio.Stream
openStream' i o f = do
    f2 <- mkProc (\_ -> faList . f . unFaList)
    Audio.openStream i o f2 nullPtr

sines = withSession $ \session -> do
    devs <- Audio.all session
    
    (flip mapM_) (unFaList devs) $ \d -> do
        putStrLn $ "    " ++ name' (castPtr d)
        return ()
    
    i <- Audio.defaultInput session
    o <- Audio.defaultOutput session
    

    -- let fmosc f r = sin' (fromRational f * sine (fromRational r))
    let osc1 = (* 0.01) $ sum $ Prelude.take 20 $ fmap (sin' . (*5) . line' . (2**)) $ [1,2-1/9..]
    let osc2 = (* 0.01) $ sum $ Prelude.take 20 $ fmap (sin' . (*5) . line' . (2**)) $ [1,2-3/9..]

    Signal.runFile (44100*10) (faList []) osc1 (faString' "hs.wav")
    
    openStream' i o (\[i1, i2] -> [osc1, osc2])
    threadDelay $ 100 * 1000000
    return ()




sine = sin' . line'
sin'      = unsafePerformIO . Signal.sin
line'     = unsafePerformIO . Signal.line
constant' = unsafePerformIO . Signal.constant
add' x y = unsafePerformIO $ Signal.add x y
sub' x y = unsafePerformIO $ Signal.subtract x y
mul' x y = unsafePerformIO $ Signal.multiply x y
div' x y = unsafePerformIO $ Signal.divide x y
instance Num Signal where
    (+) = add'
    (*) = mul'
    (-) = sub'
    fromInteger = constant' . fromInteger
instance Fractional Signal where
    (/) = div'
    fromRational = constant' . fromRational

main = do               
    setLogStd
    initialize
    putStrLn =<< unFaString =<< versionString
    sines
    terminate
    return ()


faList :: [Ptr a] -> List
faList = faList' . fmap castPtr

faList' []     = unsafePerformIO $ empty
faList' (x:xs) = unsafePerformIO $ x `cons` faList xs

unFaList :: List -> [Ptr a]
unFaList = fmap castPtr . unFaList'

unFaList' xs | isEmpty' xs = []
             | otherwise   = head' xs : unFaList (tail' xs)

head' = unsafePerformIO . List.head
tail' = unsafePerformIO . List.tail

unFaBool 0 = False
unFaBool _ = True
faBool False = 0
faBool _     = 1

name' = unsafePerformIO . unFaString . unsafePerformIO . Audio.name
isEmpty' = unFaBool . unsafePerformIO . isEmpty

faString' = unsafePerformIO . faString

faString :: String -> IO FaString
faString x = fromUtf8 =<< newCString x

unFaString :: FaString -> IO String
unFaString x = peekCString =<< toUtf8 x

withSession f = do
    session <- Audio.beginSession
    f session
    Audio.endSession session

