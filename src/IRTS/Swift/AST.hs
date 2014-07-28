{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module IRTS.Swift.AST where

import Data.Word
import Data.Char (isDigit)

import qualified Data.Text as T

data SwiftType = SwiftIntTy
            | SwiftStringTy
            | SwiftIntegerTy
            | SwiftFloatTy
            | SwiftCharTy
            | SwiftPtrTy
            | SwiftForgotTy
            deriving Eq


data SwiftInteger = SwiftBigZero
               | SwiftBigOne
               | SwiftBigInt Integer
               | SwiftBigIntExpr Swift
               deriving Eq


data SwiftNum = SwiftInt Int
           | SwiftFloat Double
           | SwiftInteger SwiftInteger
           deriving Eq


data SwiftWord = SwiftWord8 Word8
            | SwiftWord16 Word16
            | SwiftWord32 Word32
            | SwiftWord64 Word64
            deriving Eq


data SwiftAnnotation = SwiftConstructor deriving Eq


instance Show SwiftAnnotation where
  show SwiftConstructor = "constructor"


data Swift = SwiftRaw String
        | SwiftIdent String
        | SwiftFunction [String] Swift
        | SwiftType SwiftType
        | SwiftSeq [Swift]
        | SwiftReturn Swift
        | SwiftApp Swift [Swift]
        | SwiftNew String [Swift]
        | SwiftError String
        | SwiftBinOp String Swift Swift
        | SwiftPreOp String Swift
        | SwiftPostOp String Swift
        | SwiftProj Swift String
        | SwiftNull
        | SwiftUndefined
        | SwiftThis
        | SwiftTrue
        | SwiftFalse
        | SwiftArray [Swift]
        | SwiftString String
        | SwiftNum SwiftNum
        | SwiftWord SwiftWord
        | SwiftAssign Swift Swift
        | SwiftAlloc String (Maybe Swift)
        | SwiftIndex Swift Swift
        | SwiftSwitch Swift [(Swift, Swift)] (Maybe Swift)
        | SwiftCond [(Swift, Swift)]
        | SwiftTernary Swift Swift Swift
        | SwiftParens Swift
        | SwiftWhile Swift Swift
        | SwiftFFI String [Swift]
        | SwiftAnnotation SwiftAnnotation Swift
        | SwiftNoop
        deriving Eq


data FFI = FFICode Char | FFIArg Int | FFIError String

ffi :: String -> [String] -> T.Text
ffi code args = let parsed = ffiParse code in
                    case ffiError parsed of
                         Just err -> error err
                         Nothing  -> renderFFI parsed args
  where
    ffiParse :: String -> [FFI]
    ffiParse ""           = []
    ffiParse ['%']        = [FFIError $ "FFI - Invalid positional argument"]
    ffiParse ('%':'%':ss) = FFICode '%' : ffiParse ss
    ffiParse ('%':s:ss)
      | isDigit s =
         FFIArg (
           read $ s : takeWhile isDigit ss
          ) : ffiParse (dropWhile isDigit ss)
      | otherwise =
          [FFIError "FFI - Invalid positional argument"]
    ffiParse (s:ss) = FFICode s : ffiParse ss


    ffiError :: [FFI] -> Maybe String
    ffiError []                 = Nothing
    ffiError ((FFIError s):xs)  = Just s
    ffiError (x:xs)             = ffiError xs


    renderFFI :: [FFI] -> [String] -> T.Text
    renderFFI [] _ = ""
    renderFFI (FFICode c : fs) args = c `T.cons` renderFFI fs args
    renderFFI (FFIArg i : fs) args
      | i < length args && i >= 0 =
            T.pack (args !! i)
          `T.append` renderFFI fs args
      | otherwise = error "FFI - Argument index out of bounds"

compileSwift :: Swift -> T.Text
compileSwift = compileSwift' 0

compileSwift' :: Int -> Swift -> T.Text
compileSwift' indent SwiftNoop = ""

compileSwift' indent (SwiftAnnotation annotation swift) =
    "/** @"
  `T.append` T.pack (show annotation)
  `T.append` " */\n"
  `T.append` compileSwift' indent swift

compileSwift' indent (SwiftFFI raw args) =
  ffi raw (map (T.unpack . compileSwift' indent) args)

compileSwift' indent (SwiftRaw code) =
  T.pack code

compileSwift' indent (SwiftIdent ident) =
  T.pack ident

compileSwift' indent (SwiftFunction args body) =
      T.replicate indent " " `T.append` "{("
   `T.append` T.intercalate "," (map T.pack args)
   `T.append` ") -> () in\n"
   `T.append` compileSwift' (indent + 2) body
   `T.append` "\n}\n"

compileSwift' indent (SwiftType ty)
  | SwiftIntTy     <- ty = "i$Int"
  | SwiftStringTy  <- ty = "i$String"
  | SwiftIntegerTy <- ty = "i$Integer"
  | SwiftFloatTy   <- ty = "i$Float"
  | SwiftCharTy    <- ty = "i$Char"
  | SwiftPtrTy     <- ty = "i$Ptr"
  | SwiftForgotTy  <- ty = "i$Forgot"

compileSwift' indent (SwiftSeq seq) =
  T.intercalate "\n" (
    map (
      (T.replicate indent " " `T.append`) . (compileSwift' indent)
    ) $ filter (/= SwiftNoop) seq
  ) `T.append` ""

compileSwift' indent (SwiftReturn val) =
  "return " `T.append` compileSwift' indent val

compileSwift' indent (SwiftApp lhs rhs)
  | SwiftFunction {} <- lhs =
    T.concat ["(", compileSwift' indent lhs, ")(", args, ")"]
  | otherwise =
    T.concat [compileSwift' indent lhs, "(", args, ")"]
  where args :: T.Text
        args = T.intercalate "," $ map (compileSwift' 0) rhs

compileSwift' indent (SwiftNew name args) =
    ""
  `T.append` T.pack name
  `T.append` "("
  `T.append` T.intercalate "," (map (compileSwift' 0) args)
  `T.append` ")"

compileSwift' indent (SwiftError exc) =
  "(function(){throw new Error(\"" `T.append` T.pack exc `T.append` "\")})()"

compileSwift' indent (SwiftBinOp op lhs rhs) =
    compileSwift' indent lhs
  `T.append` " "
  `T.append` T.pack op
  `T.append` " "
  `T.append` compileSwift' indent rhs

compileSwift' indent (SwiftPreOp op val) =
  T.pack op `T.append` compileSwift' indent val

compileSwift' indent (SwiftProj obj field)
  | SwiftFunction {} <- obj =
    T.concat ["(", compileSwift' indent obj, ").", T.pack field]
  | SwiftAssign {} <- obj =
    T.concat ["(", compileSwift' indent obj, ").", T.pack field]
  | otherwise =
    compileSwift' indent obj `T.append` ('.' `T.cons` T.pack field)

compileSwift' indent SwiftNull =
  "nil"

compileSwift' indent SwiftUndefined =
  "undefined"

compileSwift' indent SwiftThis =
  "this"

compileSwift' indent SwiftTrue =
  "true"

compileSwift' indent SwiftFalse =
  "false"

compileSwift' indent (SwiftArray elems) =
  "[" `T.append` T.intercalate "," (map (compileSwift' 0) elems) `T.append` "]"

compileSwift' indent (SwiftString str) =
  "\"" `T.append` T.pack str `T.append` "\""

compileSwift' indent (SwiftNum num)
  | SwiftInt i                    <- num = T.pack (show i)
  | SwiftFloat f                  <- num = T.pack (show f)
  | SwiftInteger SwiftBigZero        <- num = T.pack "i$ZERO"
  | SwiftInteger SwiftBigOne         <- num = T.pack "i$ONE"
  | SwiftInteger (SwiftBigInt i)     <- num = T.pack (show i)
  | SwiftInteger (SwiftBigIntExpr e) <- num =
      "i$bigInt(" `T.append` compileSwift' indent e `T.append` ")"

compileSwift' indent (SwiftAssign lhs rhs) =
  compileSwift' indent lhs `T.append` " = " `T.append` compileSwift' indent rhs

compileSwift' 0 (SwiftAlloc name (Just val@(SwiftNew _ _))) =
  T.pack name
  `T.append` " = "
  `T.append` compileSwift' 0 val
  `T.append` "\n"

-- compileSwift' indent (SwiftAlloc name val) =
--     "var "
--   `T.append` T.pack name
--   `T.append` maybe "" ((" = " `T.append`) . compileSwift' indent) val

compileSwift' indent (SwiftAlloc name val) =
    let expr = maybe "" (compileSwift' indent) val
    in case val of (Nothing)               -> ""
                   (Just (SwiftFunction _ _)) -> (T.pack name) `T.append` " = " `T.append` expr              
                   (_)                        -> "var " `T.append` (T.pack name) 
                                                 `T.append` maybe "" ((" = " `T.append`) . compileSwift' indent) val

compileSwift' indent (SwiftIndex lhs rhs) =
    compileSwift' indent lhs
  `T.append` "["
  `T.append` compileSwift' indent rhs
  `T.append` "]"

compileSwift' indent (SwiftCond branches) =
  T.intercalate " else " $ map createIfBlock branches
  where
    createIfBlock (SwiftNoop, e@(SwiftSeq _)) =
         "{\n"
      `T.append` compileSwift' (indent + 2) e
      `T.append` "\n" `T.append` T.replicate indent " " `T.append` "}"
    createIfBlock (SwiftNoop, e) =
         "{\n"
      `T.append` compileSwift' (indent + 2) e
      `T.append` "\n" `T.append` T.replicate indent " " `T.append` "}"
    createIfBlock (cond, e@(SwiftSeq _)) =
         "if (" `T.append` compileSwift' indent cond `T.append`") {\n"
      `T.append` compileSwift' (indent + 2) e
      `T.append` "\n" `T.append` T.replicate indent " " `T.append` "}"
    createIfBlock (cond, e) =
         "if (" `T.append` compileSwift' indent cond `T.append`") {\n"
      `T.append` T.replicate (indent + 2) " "
      `T.append` compileSwift' (indent + 2) e
      `T.append` "\n"
      `T.append` T.replicate indent " "
      `T.append` "}"

compileSwift' indent (SwiftSwitch val [(_,SwiftSeq seq)] Nothing) =
  let (h,t) = splitAt 1 seq in
         (T.concat (map (compileSwift' indent) h) `T.append` "\n")
      `T.append` (
        T.intercalate "\n" $ map (
          (T.replicate indent " " `T.append`) . compileSwift' indent
        ) t
      )

compileSwift' indent (SwiftSwitch val branches def) =
     "switch(" `T.append` compileSwift' indent val `T.append` "){\n"
  `T.append` T.concat (map mkBranch branches)
  `T.append` mkDefault def
  `T.append` T.replicate indent " " `T.append` "}"
  where
    mkBranch :: (Swift, Swift) -> T.Text
    mkBranch (tag, code) =
         T.replicate (indent + 2) " "
      `T.append` "case "
      `T.append` compileSwift' indent tag
      `T.append` ":\n"
      `T.append` compileSwift' (indent + 4) code
      `T.append` "\n"
      `T.append` (T.replicate (indent + 4) " " `T.append` "")

    mkDefault :: Maybe Swift -> T.Text
    mkDefault Nothing = ""
    mkDefault (Just def) =
         T.replicate (indent + 2) " " `T.append` "default:\n"
      `T.append` compileSwift' (indent + 4)def
      `T.append` "\n"


compileSwift' indent (SwiftTernary cond true false) =
  let c = compileSwift' indent cond
      t = compileSwift' indent true
      f = compileSwift' indent false in
        "("
      `T.append` c
      `T.append` ") ? ("
      `T.append` t
      `T.append` ") : ("
      `T.append` f
      `T.append` ")"

compileSwift' indent (SwiftParens swift) =
  "(" `T.append` compileSwift' indent swift `T.append` ")"

compileSwift' indent (SwiftWhile cond body) =
     "while (" `T.append` compileSwift' indent cond `T.append` ") {\n"
  `T.append` compileSwift' (indent + 2) body
  `T.append` "\n" `T.append` T.replicate indent " " `T.append` "}"

compileSwift' indent (SwiftWord word)
  | SwiftWord8  b <- word =
      "new Uint8Array([" `T.append` T.pack (show b) `T.append` "])"
  | SwiftWord16 b <- word =
      "new Uint16Array([" `T.append` T.pack (show b) `T.append` "])"
  | SwiftWord32 b <- word =
      "new Uint32Array([" `T.append` T.pack (show b) `T.append` "])"
  | SwiftWord64 b <- word =
      "i$bigInt(\"" `T.append` T.pack (show b) `T.append` "\")"

swiftInstanceOf :: Swift -> String -> Swift
swiftInstanceOf obj cls = SwiftBinOp "is" obj (SwiftIdent cls)

swiftOr :: Swift -> Swift -> Swift
swiftOr lhs rhs = SwiftBinOp "||" lhs rhs

swiftAnd :: Swift -> Swift -> Swift
swiftAnd lhs rhs = SwiftBinOp "&&" lhs rhs

swiftMeth :: Swift -> String -> [Swift] -> Swift
swiftMeth obj meth args = SwiftApp (SwiftProj obj meth) args

swiftCall :: String -> [Swift] -> Swift
swiftCall fun args = SwiftApp (SwiftIdent fun) args

swiftTypeOf :: Swift -> Swift
swiftTypeOf swift = SwiftPreOp " is " swift

swiftEq :: Swift -> Swift -> Swift
swiftEq lhs@(SwiftNum (SwiftInteger _)) rhs = SwiftApp (SwiftProj lhs "equals") [rhs]
swiftEq lhs rhs@(SwiftNum (SwiftInteger _)) = SwiftApp (SwiftProj lhs "equals") [rhs]
swiftEq lhs rhs = SwiftBinOp "==" lhs rhs

swiftNotEq :: Swift -> Swift -> Swift
swiftNotEq lhs rhs = SwiftBinOp "!=" lhs rhs

swiftIsNumber :: Swift -> Swift
swiftIsNumber swift = (swiftTypeOf swift) `swiftEq` (SwiftString "number")

swiftIsNull :: Swift -> Swift
swiftIsNull swift = SwiftBinOp "==" swift SwiftNull

swiftCast :: Swift -> String -> Swift
swiftCast val typ = SwiftParens (SwiftBinOp "as" val (SwiftRaw typ))

swiftBigInt :: Swift -> Swift
swiftBigInt (SwiftString "0") = SwiftNum (SwiftInteger SwiftBigZero)
swiftBigInt (SwiftString "1") = SwiftNum (SwiftInteger SwiftBigOne)
swiftBigInt swift             = SwiftNum $ SwiftInteger $ SwiftBigIntExpr swift

swiftUnPackBits :: Swift -> Swift
swiftUnPackBits swift = SwiftIndex swift $ SwiftNum (SwiftInt 0)

swiftPackUBits8 :: Swift -> Swift
swiftPackUBits8 swift = SwiftNew "Uint8Array" [SwiftArray [swift]]

swiftPackUBits16 :: Swift -> Swift
swiftPackUBits16 swift = SwiftNew "Uint16Array" [SwiftArray [swift]]

swiftPackUBits32 :: Swift -> Swift
swiftPackUBits32 swift = SwiftNew "Uint32Array" [SwiftArray [swift]]

swiftPackSBits8 :: Swift -> Swift
swiftPackSBits8 swift = SwiftNew "Int8Array" [SwiftArray [swift]]

swiftPackSBits16 :: Swift -> Swift
swiftPackSBits16 swift = SwiftNew "Int16Array" [SwiftArray [swift]]

swiftPackSBits32 :: Swift -> Swift
swiftPackSBits32 swift = SwiftNew "Int32Array" [SwiftArray [swift]]

