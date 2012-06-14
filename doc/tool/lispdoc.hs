#!/usr/bin/env runhaskell

--  Converts inline Lisp documentation to Doxygen/Markdown modules
--  Requires the Haskell platform + pandoc and pandoc-types from hackage.org

--  Copyright (c) Hans Hoglund 2012

import Control.Monad
import Data.Monoid
import System.Environment (getArgs)
import qualified Data.List as List
import Data.List.HT (replace)

import Text.Parsec.Prim (Parsec)
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Token hiding (symbol)
import Text.Parsec.String

import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import Text.Pandoc.Definition    
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.Markdown

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool
    deriving (Eq, Show)


lispDef = 
    LanguageDef 
    {
        commentStart =  "#|",
        commentEnd =  "|#",
        commentLine =  ";",
        nestedComments =  False,
        identStart =  (letter <|> symbol),
        identLetter =  (letter <|> digit <|> symbol),
        opStart =  mzero,
        opLetter =  mzero,
        reservedNames =  mzero,
        reservedOpNames =  mzero,
        caseSensitive =  True
    }  
lexer = makeTokenParser lispDef

symbol :: Parser Char
symbol = oneOf "!$%&*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ Atom atom

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit                          

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr (whiteSpace lexer)

parseQuoted :: Parser LispVal
parseQuoted = do
    optional (char '#')
    oneOf "'`,"
    x <- parseExpr
    return $ List [Atom "quote", x]

parseFile :: Parser [LispVal]
parseFile = do optional (whiteSpace lexer)
               sepEndBy parseExpr (whiteSpace lexer)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList
               char ')'
               return x 
                       
-- parseFromFile parseFile "../../audio.lisp"

createDoc :: LispVal -> [LispDoc]
createDoc x
    | isDef "defclass" x          =  return $ lispClass     (defName x) [] [] (docElem x)
    | isDef "define-condition" x  =  return $ lispCondition (defName x) [] (docElem x)
    | isDef "defgeneric" x        =  return $ lispGeneric   (defName x) [] (docElem x)
    | isDef "defmethod" x         =  return $ lispGeneric   (defName x) [] (docString x)
    | isDef "defun" x             =  return $ lispFunction  (defName x) (docString x)
    | isDef "defmacro" x          =  return $ lispMacro     (defName x) (docString x)
    | isDef "defconstant" x       =  return $ lispConstant  (defName x) ""
    -- | isList x                    =  concat . map createDoc . getList $ x
    | otherwise                   =  mzero

defName :: LispVal -> String
defName (List (_:(Atom name):_)) = name
defName _                        = ""

docString :: LispVal -> String
docString (List xs)
    | length xs >= 4 && isString (xs !! 3) = getString (xs !! 3)
    | otherwise                            = ""  

docElem :: LispVal -> String
docElem (List xs) =
    maybe "" (\(List xs) -> getString (xs !! 1)) $ List.find (isDef ":documentation") xs

isList :: LispVal -> Bool
isList (List _) = True
isList _        = False

getList :: LispVal -> [LispVal]
getList (List xs) = xs

isAtom :: LispVal -> Bool
isAtom (Atom _) = True
isAtom _        = False

getAtom :: LispVal -> String
getAtom (Atom x) = x

isString :: LispVal -> Bool
isString (String _) = True
isString _          = False

getString :: LispVal -> String
getString (String x) = x


isDef :: String -> LispVal -> Bool
isDef defType (List ((Atom listHead):_))  =  listHead == defType
isDef defType _                           =  False

lispClass n p m d   = Class     $ LispClass n p m d
lispCondition n p d = Condition $ LispCondition n p d
lispGeneric n c d   = Generic   $ LispGeneric n c d
lispFunction n d    = Function  $ LispFunction n d
lispMacro n d       = Macro     $ LispMacro n d
lispConstant n d    = Constant  $ LispConstant n d
lispDefine n d      = Define    $ LispDefine n d

data LispClass = 
    LispClass { className :: String,
                classParents :: [LispClass],
                classMethods :: [LispGeneric],
                classDocumentation :: String } 
                deriving (Eq, Show, Ord)


data LispCondition = 
    LispCondition { conditionName :: String,
                    conditionParents :: [LispCondition],
                    conditionDocumentation :: String }
                    deriving (Eq, Show, Ord)

data LispGeneric =
    LispGeneric { genericName :: String,
                          genericClasses :: [LispClass],
                          genericDocumentation :: String }
                          deriving (Eq, Show, Ord)

data LispFunction =
    LispFunction { functionName :: String,
                   functionDocumentation :: String }
                   deriving (Eq, Show, Ord)

data LispMacro = LispMacro { macroName :: String,
                             macroDocumentation :: String }
                             deriving (Eq, Show, Ord)

data LispConstant = LispConstant { constantName :: String,
                                   constantDocumentation :: String }
                                   deriving (Eq, Show, Ord)

data LispDefine = LispDefine { defineName :: String,
                               defineDocumentation :: String }
                               deriving (Eq, Show, Ord)

data LispDoc 
    = Class LispClass
    | Condition LispCondition
    | Generic LispGeneric
    | Function LispFunction
    | Macro LispMacro
    | Constant LispConstant
    | Define LispDefine            
    -- internal
    | Section Int String
    deriving (Eq, Show, Ord)

isClass     (Class _)       = True
isClass     _               = False
isCondition (Condition _)   = True
isCondition _               = False
isGeneric   (Generic _)     = True
isGeneric   _               = False
isFunction  (Function _)    = True
isFunction  _               = False
isMacro     (Macro _)       = True
isMacro     _               = False
isConstant  (Constant _)    = True
isConstant  _               = False
isDefine    (Define _)      = True
isDefine    _               = False

renderDoc :: LispDoc -> Pandoc
renderDoc (Class x)     = Pandoc mempty $ [Header kNameHeading [Str $ className x]] ++ parseDoc (classDocumentation x)
renderDoc (Condition x) = Pandoc mempty $ [Header kNameHeading [Str $ conditionName x]] ++ parseDoc (conditionDocumentation x)
renderDoc (Generic x)   = Pandoc mempty $ [Header kNameHeading [Str $ genericName x]] ++ parseDoc (genericDocumentation x)
renderDoc (Function x)  = Pandoc mempty $ [Header kNameHeading [Str $ functionName x]] ++ parseDoc (functionDocumentation x)
renderDoc (Macro x)     = Pandoc mempty $ [Header kNameHeading [Str $ macroName x]] ++ parseDoc (macroDocumentation x)
renderDoc (Constant x)  = Pandoc mempty $ [Header kNameHeading [Str $ constantName x]] ++ parseDoc (constantDocumentation x)
renderDoc (Section n x) = Pandoc mempty $ [Header n [Str x]]
renderDoc _             = mempty

parseDoc :: String -> [Block]
parseDoc str = blocks
    where
        (Pandoc _ blocks) = readMarkdown defaultParserState str 

instance Monoid Meta where
    mempty = Meta [] [] []
    x `mappend` y = x
    
instance Monoid Pandoc where
    mempty = Pandoc mempty []
    Pandoc m xs `mappend` Pandoc _ ys = Pandoc m (xs ++ ys)

addSections :: [LispDoc] -> [LispDoc]
addSections xs = mempty
    ++ return (Section kSectionHeading "Classes")
    ++ filter isClass xs
    ++ return (Section kSectionHeading "Conditions")
    ++ filter isCondition xs
    ++ return (Section kSectionHeading "Generic functions")
    ++ filter isGeneric xs
    ++ return (Section kSectionHeading "Functions")
    ++ filter isFunction xs
    ++ return (Section kSectionHeading "Macros")
    ++ filter isMacro xs
    ++ return (Section kSectionHeading "Constants")
    ++ filter isConstant xs

renderAllDocs :: [LispDoc] -> String
renderAllDocs = pre . rep . writeMarkdown defaultWriterOptions { writerTableOfContents = True } . mconcat . map renderDoc . addSections
    where        
        -- rep = id   
        pre = ("@defgroup sclaudiol Lisp API \n@ingroup sclaudiol\n\n" ++)
        rep = replace "@ref%20" "@ref "

main' :: FilePath -> IO ()
main' path = do
    (Right lispVal) <- parseFromFile parseFile path
    putStr . renderAllDocs . concatMap createDoc $ lispVal


kSectionHeading = 2
kNameHeading    = 3


main = do
    args <- getArgs
    main' (args !! 0)
    
    
