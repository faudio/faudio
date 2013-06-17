#!/usr/bin/env runhaskell

module Main where
import Data.List
import Text.Printf

-- 
-- add   T1      r1 r2              r1       := r1 + r2
-- sub   T1      r1 r2              r1       := r1 - r2
-- mul   T1      r1 r2              r1       := r1 * r2
-- div   T1      r1 r2              r1       := r1 / r2
-- rem   T1      r1 r2              r1       := r1 % r2
-- bool  T1      r1 r2              r1       := case r2 of 0 -> 0, _ -> 1
-- not   T1      r1 r2              r1       := case r2 of 0 -> 1, _ -> 0
-- and   T1      r1 r2              r1       := r1 & r2
-- or    T1      r1 r2              r1       := r1 | r2
-- xor   T1      r1 r2              r1       := r1 ^ r2
-- 
-- eq    T1      r1 r2              r1       := r1 == r2
-- ne    T1      r1 r2              r1       := r1 != r2
-- lt    T1      r1 r2              r1       := r1 < r2
-- gt    T1      r1 r2              r1       := r1 > r2
-- lte   T1      r1 r2              r1       := r1 <= r2
-- gte   T1      r1 r2              r1       := r1 >= r2



types = [
    ("i8",  "uint8_t"),
    ("i16", "uint16_t"),
    ("i32", "uint32_t"),
    ("i64", "uint64_t"),
    ("f32", "float"),
    ("f64", "double")
  ]

ops = [
    ("add", "+"),
    ("sub", "-"),
    ("mul", "*"),
    ("div", "/"),
    ("rem", "%"),
    ("and", "&"),
    ("or",  "|"),
    ("xor", "^"),

    ("eq",  "=="),
    ("ne",  "!="),
    ("lt",  "<"),
    ("gt",  ">"),
    ("lte", "<="),
    ("gte", ">=")
  ]

res :: (String,String) -> (String,String) -> String
res (opn, op) (tn, t) = printf "LLM_NV_OP(%-4s, %-3s, %-2s, %-2s, %-8s, %-8s);" opn op tn tn t t


main = do
    let r = [res op t | t  <- types, op <- ops ] 
    putStrLn $ concat $ intersperse "\n" $ r
    
    
