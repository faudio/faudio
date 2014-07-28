
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Flisp where

import Data.Maybe
import Data.String
import Data.Monoid
import   Data.Attoparsec.Number
import Data.AttoLisp
import qualified Data.Attoparsec.ByteString as P
import qualified Data.List as L
import qualified Data.List.Split as LS
import Data.Text(unpack)

main = do
  !inp <- getContents
  -- putStrLn "/*"
  -- putStrLn inp
  -- putStrLn "*/"
  putStrLn ""
  putStrLn $ L.intercalate "\n\n" $ translateFlispDefs inp
  putStrLn ""
  -- putStrLn "flisp"


data Macro = Macro {
 _name :: String,
 _vars :: [String],
 _body :: Lisp
 }
 deriving (Eq, Ord, Show)
-- TODO variadic


deepExpandMacros :: [Macro] -> Lisp -> Lisp
deepExpandMacros m l@(List xs) = expandMacros m $ List $ fmap (deepExpandMacros m) xs
deepExpandMacros m l           = expandMacros m l

-- expand all macros at the top level
expandMacros :: [Macro] -> Lisp -> Lisp
expandMacros = foldr (.) id . fmap expandMacro

expandMacro :: Macro -> Lisp -> Lisp
expandMacro m x = fromMaybe x $ applyMacro m x

{-
For the macro to apply the *form* must
  - Be a list starting with the name of the macro
  - If the macro is non-variadic
    - Have exactly the same number of arguments as the macro
  - Otherwise
    - Have the at least the same number of arguments as the macro (excluding the ... wildcard)
-}
applyMacro :: Macro -> Lisp -> Maybe Lisp
applyMacro (Macro n vs b) (List ((Symbol n') : ps))
  | n == unpack n' && length vs == length ps
  = Just $ deepSubst (zip vs ps) b
applyMacro _ _ = Nothing
-- TODO variadic

deepSubst :: [(String, Lisp)] -> Lisp -> Lisp
deepSubst s l@(List xs) = subst s $ List (fmap (deepSubst s) xs)
deepSubst s l           = subst s l

subst :: [(String, Lisp)] -> Lisp -> Lisp
subst = foldr (.) id . map subst1

subst1 :: (String, Lisp) -> Lisp -> Lisp
subst1 (s, t) o@(Symbol s')
  | s == unpack s' = t
  | otherwise      = o
subst1 _      o = o

-- List [Symbol "defmacro"]

-- TODO partial
-- TODO variadic
parseMacro :: Lisp -> Maybe Macro
parseMacro (List (Symbol "defmacro" : Symbol n : List vs : [b])) = Just $ Macro (unpack n) (fmap getSymbol vs) b
  where
    getSymbol = show
parseMacro _ = Nothing





-- Minimal C AST
type CName = String
data CType
  = CPtr CType
  | CConst CType
  | CType String
  deriving (Eq, Ord, Show)
data CDecl
  = CInclude String
  | CTypedefD CName CType
  | CStructD CName [(CDecl, String)]
  | CFuncD CName CType [(CName, CType)] [CStm]
  deriving (Eq, Ord, Show)
data CStm
  = CBlock [CStm]
  | CIf CExpr CStm (Maybe CStm)
  | CWhile CExpr CStm
  | CDecl CType CName
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
    showType (CPtr x) = showType x ++ "*"
    showType (CConst x) = "const " ++ showType x
    showType (CType x) = x
    
    showDecl (CInclude e) = "#include <" ++ e ++ ">"
    showDecl (CTypedefD n t) = "typedef " ++ showType t ++ n
    showDecl (CStructD n xs) = "struct {}" -- TODO
    showDecl (CFuncD n rt ps b) = showType rt ++ " " ++ n 
      ++ "(" ++ L.intercalate "," (map (\(n,t) -> showType t ++ " " ++ n) ps) ++ ")"
      ++ showStm (CBlock b)

    showStm (CBlock b)  = "{\n" ++ L.intercalate "\n" (map showStm b) ++ "\n}"
    showStm (CIf p a b) = "if (" ++ showExpr p ++ ") " ++ showStm a ++ maybe "" ((" else " ++ ) . showStm) b
    showStm (CWhile p a) = "while (" ++ showExpr p ++ ") " ++ showStm a
    showStm (CDecl t n) = showType t ++ " " ++ n ++ ";"
    showStm (CAssign t n e) = maybe "" ((++ " ").showType) t ++ n ++ " = " ++ showExpr e ++ ";"
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
compilePrimE :: Lisp -> CExpr
compilePrimE = go 
 where
  -- Literals
  go (Symbol "nil") = CVar "false"
  go (Symbol "t")   = CVar "true"
  go (Number (I n)) = CInt n
  go (Number (D n)) = CFloat n
  go (String s)     = CApp "fa_string_from_utf8" [CStr ((compileName.unpack) s)]
  go (List [Symbol "c-string", String s]) = CStr ((compileName.unpack) s)

  -- Single variable
  go (Symbol n) = CVar ((compileName.unpack) n)
  
  -- Operators
  go (List [Symbol "not", x])       = COp1 "!" (go x)
  go (List [Symbol "negate", x])    = COp1 "-" (go x)
  go (List [Symbol "and", x, y])    = COp2 "&&" (go x) (go y)
  go (List [Symbol "or", x, y])     = COp2 "||" (go x) (go y)
  go (List [Symbol "+", x, y])      = COp2 "+" (go x) (go y)
  go (List [Symbol "-", x, y])      = COp2 "-" (go x) (go y)
  go (List [Symbol "*", x, y])      = COp2 "*" (go x) (go y)
  go (List [Symbol "/", x, y])      = COp2 "/" (go x) (go y)
  go (List [Symbol "%", x, y])      = COp2 "%" (go x) (go y)
  go (List [Symbol "<", x, y])      = COp2 "<" (go x) (go y)
  go (List [Symbol ">", x, y])      = COp2 ">" (go x) (go y)
  go (List [Symbol "<=", x, y])     = COp2 "<=" (go x) (go y)
  go (List [Symbol ">=", x, y])     = COp2 ">=" (go x) (go y)
  go (List [Symbol "if", x, y, z])  = COp3 "?:" (go x) (go y) (go z)

  go (List [Symbol "+~", x, y])  = CApp "fa_add" [go x, go y]
  go (List [Symbol "-~", x, y])  = CApp "fa_subtract" [go x, go y]
  go (List [Symbol "*~", x, y])  = CApp "fa_multiply" [go x, go y]
  go (List [Symbol "/~", x, y])  = CApp "fa_divide" [go x, go y]

  go (List [Symbol "<~", x, y])  = CApp "fa_less_than" [go x, go y]
  go (List [Symbol ">~", x, y])  = CApp "fa_greater_than" [go x, go y]
  go (List [Symbol "<=~", x, y]) = CApp "fa_less_than_equal" [go x, go y]
  go (List [Symbol ">=~", x, y]) = CApp "fa_greater_than_equal" [go x, go y]

  -- Function
  go (List (Symbol n : xs))      = CApp ((compileName.unpack) n) (fmap go xs)
  go x = error $ "compilePrim: Unknown form " ++ show x


compilePrimS :: Lisp -> CStm
compilePrimS = go
  where
    go (List (Symbol "defvar":Symbol n:[]))   = CDecl (defaultType) (compileName.unpack$ n)
    go (List (Symbol "defvar":Symbol n:t:[])) = CDecl (compilePrimT t) (compileName.unpack$ n)
    go (List (Symbol "defvar":Symbol n:t:v:[])) = CAssign (Just $ compilePrimT t) (compileName.unpack$ n) (compilePrimE v)
    
    go (List (Symbol "return":x:[]))       = CReturn (compilePrimE x)
    go (List (Symbol "progn":xs))          = CBlock (fmap compilePrimS xs)
    go (List [Symbol "setf", Symbol n, y]) = CAssign Nothing ((compileName.unpack) n) (compilePrimE y)
    go (List [Symbol "setq", Symbol n, y]) = CAssign Nothing ((compileName.unpack) n) (compilePrimE y)
    
    go (List (Symbol "c-if":p:a:[]))       = CIf (compilePrimE p) (compilePrimS a) Nothing
    go (List (Symbol "c-if":p:a:b:[]))     = CIf (compilePrimE p) (compilePrimS a) (Just $ compilePrimS b)
    go (List (Symbol "c-if":_))            = error "Strange if"
    go (List (Symbol "c-while":p:a:[]))    = CWhile (compilePrimE p) (compilePrimS a)
    -- TODO switch
    go (List (Symbol "c-switch":e:List cs:[])) = undefined
    go x = CExpr $ compilePrimE x

-- TODO function pointers etc
compilePrimT :: Lisp -> CType
compilePrimT = go
  where
    go (Symbol t) = prim t
    go (List [Symbol ":pointer", t]) = CPtr (go t)
    go (List [Symbol ":const", t]) = CConst (go t)
    
    prim ":int"         = CType "int"
    prim ":uint"        = CType "unsigned int"
    prim ":int32"       = CType "int32_t"
    prim ":uint32"      = CType "uint32_t"
    prim ":int64"       = CType "int64_t"
    prim ":uint64"      = CType "uint64_t"
    prim ":float"       = CType "float"
    prim ":double"      = CType "double"
    prim ":char"        = CType "char"
    prim ":uchar"       = CType "uchar"
    prim "string-type"  = CType "fa_string_t"
    prim "ptr-type"     = CType "fa_ptr_t"
    prim "list-type"     = CType "fa_list_t"
    prim "signal-type"     = CType "fa_signal_t"

compilePrimD :: Lisp -> CDecl
-- (defun foo (x))
compilePrimD (List (Symbol "defun" : Symbol n : List ps : body))                = CFuncD (compileName.unpack $ n) (defaultType) (fmap compileParam ps) (compileBody body)
compilePrimD (List (Symbol "defun" : Symbol n : t@(Symbol _) : List ps : body)) = CFuncD (compileName.unpack $ n) (compilePrimT t) (fmap compileParam ps) (compileBody body)
compilePrimD (List (Symbol "include" : String n : []))                          = CInclude (unpack n)
compilePrimD x = error $ "compilePrimD: Unknown form " ++ show x

compileParam :: Lisp -> (String, CType)
compileParam = go
  where
    go (Symbol x) = (compileName.unpack $ x, defaultType)
    go (List [Symbol x, t]) = (compileName.unpack $ x,compilePrimT t)

compileBody :: [Lisp] -> [CStm]
compileBody []  = error "Empty body"
compileBody xs  = fmap compilePrimS (init xs) ++ [CReturn $ compilePrimE $ last xs]

defaultType :: CType
defaultType = CType "fa_ptr_t"

compileName :: String -> String
compileName = id
  . replace ":" "_"
  . replace "-" "_"
  . replace "-type" "_t"
  . replace "*" "__"
  -- . replaceExact "-" "subtract" 
  -- . replaceExact "+" "add"
  -- . replace "-" "subtract"
  -- . replaceExact "*" "multiply"
  . checkReserved
  where
    replaceExact o n x = if x == o then n else x
    replace o n = L.intercalate n . LS.splitOn o
    checkReserved x = if x `elem` res then error ("Reserved Name: '" ++ x ++ "', possibly by using a statement in expression context") else x
    res = ["not", "negate", "and", "or", "if", "c-if", "c-while", "progn", "setf"]

    
translateFlispDefs :: String -> [String]
translateFlispDefs x = case P.parse lisp ("(" <> fromString x <> ")") of
  P.Done _ (List rs) -> fmap showC . fmap compilePrimD . compileWithMacros $ rs
  _                  -> error "Parse error"

compileWithMacros :: [Lisp] -> [Lisp]
compileWithMacros as = let (m, xs) = separateMacros as
  in fmap (rdeepExpandMacros m) xs

compileWithoutMacros :: [Lisp] -> [Lisp]
compileWithoutMacros as = let (m, xs) = separateMacros as in xs

separateMacros :: [Lisp] -> ([Macro],[Lisp])
separateMacros = mconcat . fmap g
  where
    g x = case parseMacro x of
      Nothing -> ([],[x])
      Just m  -> ([m],[])

-- Deep expand macros until no macro applies
rdeepExpandMacros :: [Macro] -> Lisp -> Lisp
rdeepExpandMacros m x = 
  let x2 = deepExpandMacros m x in
    if   x2 == x then x
    else rdeepExpandMacros m x2

rdeepExpandMacrosStep :: [Macro] -> Lisp -> [Lisp]
rdeepExpandMacrosStep m x = 
  let x2 = deepExpandMacros m x in
    if   x2 == x then [x]
    else x : rdeepExpandMacrosStep m x2

-- -- Translate singlel definition
-- translateFlispDef :: String -> String
-- translateFlispDef x = case P.parse lisp (fromString x) of
--   P.Done _ r -> showC $ compilePrimD r
--   _          -> error "Parse error"

compileFlisp :: [Macro] -> [Lisp] -> [CDecl]
compileFlisp _ = undefined




