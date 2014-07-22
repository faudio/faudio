
{-# LANGUAGE OverloadedStrings #-}

module Flisp where

import   Data.Attoparsec.Number
import Data.AttoLisp
import qualified Data.Attoparsec.ByteString as P
import qualified Data.List as L
import Data.Text(unpack)

main = putStrLn "flisp"


type Macro = Lisp -> Lisp

-- Minimal C AST
type CName = String
data CType
  = CPtr CType
  | CType String
  deriving (Eq, Ord, Show)
data CDecl
  = CTypedefD CName CType
  | CStructD CName [(CDecl, String)]
  | CFuncD CName CType [(CName, CType)] [CStm]
  deriving (Eq, Ord, Show)
data CStm
  = CBlock [CStm]
  | CIf CExpr CStm (Maybe CStm)
  | CWhile CExpr CStm
  | CAssign (Maybe CType) CName CExpr
  | CExpr CExpr
  | CReturn CExpr
  deriving (Eq, Ord, Show)
data CExpr
  = COp1 CName CExpr
  | COp2 CName CExpr CExpr
  | COp3 CName CExpr CExpr CExpr
  | CApp CName [CExpr]
  | CVar CName
  | CCast CType CExpr
  | CInt Integer
  | CFloat Double
  | CStr String
  deriving (Eq, Ord, Show)

showC :: CDecl -> String
showC = showDecl
  where
    showType (CPtr x) = "*" ++ showType x
    showType (CType x) = x
    showDecl (CTypedefD n t) = "typedef " ++ showType t ++ n
    showDecl (CStructD n xs) = "struct {}" -- TODO
    showDecl (CFuncD n rt ps b) = showType rt ++ " " ++ n 
      ++ "(" ++ L.intercalate "," (map (showType . snd) ps) ++ ")"
      ++ showStm (CBlock b)
    showStm (CBlock b)  = "{\n" ++ L.intercalate "\n" (map showStm b) ++ "\n}"
    showStm (CIf p a b) = "if (" ++ showExpr p ++ ") " ++ showStm a ++ maybe "" showStm b
    showStm (CWhile p a) = "while (" ++ showExpr p ++ ") " ++ showStm a
    showStm (CAssign t n e) = maybe "" ((++ " ").showType) t ++ "" ++ n ++ " = " ++ showExpr e ++ ";"
    showStm (CExpr e) = showExpr e ++ ";"
    showStm (CReturn e) = "return " ++ showExpr e ++ ";"
    showExpr (COp1 n a) = n ++ showExpr a
    showExpr (COp2 n a b) = showExpr a ++ n ++ showExpr b
    showExpr (COp3 _ a b c) = showExpr a ++ "?" ++ showExpr b ++ ":" ++ showExpr c
    showExpr (CApp n e) = n ++ "(" ++ L.intercalate ","  (map showExpr e) ++ ")"
    showExpr (CVar n) = n
    showExpr (CCast t a) = "(" ++ showType t ++ ")" ++ showExpr a
    showExpr (CInt e)   = show e
    showExpr (CFloat e) = show e
    showExpr (CStr e)   = show e
-- compilePrim (Symbol t) = 

{-
Primitives
  not and or + - * % negate
  setf                        ; assignment
  ""                          ; string
  (c-string "")               ; c string
  (coerce 2 :int)             ; cast
  if while case

  ; default to pointer
  (defun foobar (x y) 
    ""
    ...)

  (defun prefix:foo-bar :void ((x :int) y)
    ""
    ...)

  (defun :inline :static fa:foobar :void ((x :int) y)
    ""
    ...)

  (deftype unary-type (:ptr (:ptr :ptr)))     ; MUST end with type

  (defstruct node
    (count :size-type                      
      "Number of references")
    (next (:pointer (:struct :node))
      "Number of references"))

-}
compilePrim :: Lisp -> CExpr
compilePrim (List [Symbol "not", x])    = COp1 "!" (compilePrim x)
compilePrim (List [Symbol "negate", x])    = COp1 "-" (compilePrim x)
compilePrim (List [Symbol "and", x, y]) = COp2 "&&" (compilePrim x) (compilePrim y)
compilePrim (List [Symbol "or", x, y])  = COp2 "||" (compilePrim x) (compilePrim y)
compilePrim (List [Symbol "+", x, y])   = COp2 "+" (compilePrim x) (compilePrim y)
compilePrim (List [Symbol "-", x, y])   = COp2 "-" (compilePrim x) (compilePrim y)
compilePrim (List [Symbol "*", x, y])   = COp2 "*" (compilePrim x) (compilePrim y)
compilePrim (List [Symbol "/", x, y])   = COp2 "/" (compilePrim x) (compilePrim y)
compilePrim (List [Symbol "%", x, y])   = COp2 "%" (compilePrim x) (compilePrim y)
compilePrim (List (Symbol n : xs))      = CApp (unpack n) (fmap compilePrim xs)

compilePrim (String s)                  = CApp "__tostring__" [CStr (unpack s)]
compilePrim (List [Symbol "c-string", String s]) = CStr (unpack s)
compilePrim (Number (I n)) = CInt n
compilePrim (Number (D n)) = CFloat n
compilePrim (Symbol n) = CVar (unpack n)

compilePrimS (List (Symbol "progn":xs))          = CBlock (fmap compilePrimS xs)
compilePrimS (List [Symbol "setf", Symbol n, y]) = CAssign Nothing (unpack n) (compilePrim y)
compilePrimS x = CExpr $ compilePrim x

compilePrimT _ = CType "ptr_t"

compilePrimD (List (Symbol "defun" : Symbol n : t : body))
  = CFuncD (unpack n) (CType "ptr_t") [] (fmap compilePrimS body)
  -- TODO translate n

test = case P.parse lisp "(defun foo (a b c) (progn (+ 1 (+ 2 3)) (setf foppa 1) (print \"foo\") 3))" of
  P.Done _ r -> putStrLn $ showC $ compilePrimD r
  _          -> error "Parse error"

compileFlisp :: [Macro] -> [Lisp] -> [CDecl]
compileFlisp _ = undefined