{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module IRTS.CodegenSwift (codegenSwift, SwiftTarget(..)) where

import IRTS.Swift.AST

import Idris.AbsSyntax hiding (TypeCase)
import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.CodegenCommon
import Idris.Core.TT
import IRTS.System
import Util.System

import Control.Arrow
import Control.Monad (mapM)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.RWS hiding (mapM)
import Control.Monad.State
import Data.Char
import Numeric
import Data.List
import Data.Maybe
import Data.Word
import Data.Traversable hiding (mapM)
import System.IO
import System.Directory

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


data CompileInfo = CompileInfo { compileInfoApplyCases  :: [Int]
                               , compileInfoEvalCases   :: [Int]
                               , compileInfoNeedsBigInt :: Bool
                               }


initCompileInfo :: [(Name, [BC])] -> CompileInfo
initCompileInfo bc =
  CompileInfo (collectCases "APPLY" bc) (collectCases "EVAL" bc) (lookupBigInt bc)
  where
    lookupBigInt :: [(Name, [BC])] -> Bool
    lookupBigInt = any (needsBigInt . snd)
      where
        needsBigInt :: [BC] -> Bool
        needsBigInt bc = or $ map testBCForBigInt bc
          where
            testBCForBigInt :: BC -> Bool
            testBCForBigInt (ASSIGNCONST _ c)  =
              testConstForBigInt c

            testBCForBigInt (CONSTCASE _ c d) =
                 maybe False needsBigInt d
              || (or $ map (needsBigInt . snd) c)
              || (or $ map (testConstForBigInt . fst) c)

            testBCForBigInt _ = False

            testConstForBigInt :: Const -> Bool
            testConstForBigInt (BI _)  = True
            testConstForBigInt (B64 _) = True
            testConstForBigInt _       = False


    collectCases :: String ->  [(Name, [BC])] -> [Int]
    collectCases fun bc = getCases $ findFunction fun bc

    findFunction :: String -> [(Name, [BC])] -> [BC]
    findFunction f ((MN 0 fun, bc):_)
      | fun == txt f = bc
    findFunction f (_:bc) = findFunction f bc

    getCases :: [BC] -> [Int]
    getCases = concatMap analyze
      where
        analyze :: BC -> [Int]
        analyze (CASE _ _ b _) = map fst b
        analyze _              = []


data SwiftTarget = Swift deriving Eq

codegenSwift :: CodeGenerator
codegenSwift ci =
  codegenSwift_all Swift (simpleDecls ci)
    (includes ci) [] (outputFile ci) (outputType ci)

codegenSwift_all
  :: SwiftTarget
  -> [(Name, SDecl)]
  -> [FilePath]
  -> [String]
  -> FilePath
  -> OutputType
  -> IO ()
codegenSwift_all target definitions includes libs filename outputType = do
  let bytecode = map toBC definitions
  let info = initCompileInfo bytecode
  let swift = concatMap (translateDecl info) bytecode
  let full = concatMap processFunction swift
  let code = deadCodeElim full
  let (cons, opt) = optimizeConstructors code
  let (header, rt) = case target of
                          Swift -> ("", "")

  included   <- concat <$> getIncludes includes
  path       <- (++) <$> getDataDir <*> (pure "/swiftrts/")
  idrRuntime <- readFile $ path ++ "Runtime-common.swift"
  -- tgtRuntime <- readFile $ concat [path, "Runtime", rt, ".swift"]
  -- swiftbn       <- if compileInfoNeedsBigInt info
  --                  then readFile $ path ++ "swiftbn/swiftbn.swift"
  --                  else return ""
  let runtime = (  header
                ++ includeLibs libs
                ++ included
                -- ++ swiftbn
                ++ idrRuntime
                -- ++ tgtRuntime
                )
  TIO.writeFile filename (  T.pack runtime
                         `T.append` T.concat (map varDecl opt)
                         `T.append` T.pack "\n\n"
                         `T.append` T.concat (map varDecl cons)
                         `T.append` T.pack "\n\n"
                         `T.append` T.concat (map compileSwift opt)
                         `T.append` T.pack "\n\n"
                         `T.append` T.concat (map compileSwift cons)
                         `T.append` T.pack "\n\n"
                         `T.append` main
                         `T.append` invokeMain
                         )
  setPermissions filename (emptyPermissions { readable   = True
                                            , executable = False
                                            , writable   = True
                                            })
    where
      varDecl :: Swift -> T.Text
      varDecl (SwiftAlloc name (Just (SwiftFunction _ _))) = T.pack $ "internal var " ++ name ++ ": i$Function\n"
      varDecl (SwiftAlloc name _) = T.pack $ "var " ++ name ++ ": i$CON\n"
      varDecl _ = ""
            
      deadCodeElim :: [Swift] -> [Swift]
      deadCodeElim swift = concatMap collectFunctions swift
        where
          collectFunctions :: Swift -> [Swift]
          collectFunctions fun@(SwiftAlloc name _)
            | name == translateName (sMN 0 "runMain") = [fun]

          collectFunctions fun@(SwiftAlloc name (Just (SwiftFunction _ body))) =
            let invokations = sum $ map (
                    \x -> execState (countInvokations name x) 0
                  ) swift
             in if invokations == 0
                   then []
                   else [fun]

          countInvokations :: String -> Swift -> State Int ()
          countInvokations name (SwiftAlloc _ (Just (SwiftFunction _ body))) =
            countInvokations name body

          countInvokations name (SwiftSeq seq) =
            void $ traverse (countInvokations name) seq

          countInvokations name (SwiftAssign _ rhs) =
            countInvokations name rhs

          countInvokations name (SwiftCond conds) =
            void $ traverse (
                runKleisli $ arr id *** Kleisli (countInvokations name)
              ) conds

          countInvokations name (SwiftSwitch _ conds def) =
            void $ traverse (
              runKleisli $ arr id *** Kleisli (countInvokations name)
            ) conds >> traverse (countInvokations name) def

          countInvokations name (SwiftApp lhs rhs) =
            void $ countInvokations name lhs >> traverse (countInvokations name) rhs

          countInvokations name (SwiftNew _ args) =
            void $ traverse (countInvokations name) args

          countInvokations name (SwiftArray args) =
            void $ traverse (countInvokations name) args

          countInvokations name (SwiftIdent name')
            | name == name' = get >>= put . (+1)
            | otherwise     = return ()

          countInvokations _ _ = return ()

      processFunction :: Swift -> [Swift]
      processFunction =
        collectSplitFunctions . (\x -> evalRWS (splitFunction x) () 0)

      includeLibs :: [String] -> String
      includeLibs =
        concatMap (\lib -> "var " ++ lib ++ " = require(\"" ++ lib ++"\");\n")

      getIncludes :: [FilePath] -> IO [String]
      getIncludes = mapM readFile

      main :: T.Text
      main =
        compileSwift $ SwiftAlloc "var main" (Just $
          SwiftFunction [] (
            case target of
                 Swift -> mainFun
          )
        )

      mainFun :: Swift
      mainFun =
        SwiftSeq [ SwiftAlloc "vm" (Just $ SwiftNew "I$VM" [])
              , SwiftApp (SwiftIdent "i$SCHED") [SwiftIdent "vm"]
              , SwiftApp (
                  SwiftIdent (translateName (sMN 0 "runMain"))
                ) $ replicate 2 (SwiftNum (SwiftInt 0))
              , SwiftWhile (SwiftProj swiftCALLSTACK "count > 0") (
                  SwiftSeq [ SwiftAlloc "funcWrapper" $ Just (swiftCast swiftPOP "i$FuncWrapper")
                           , SwiftAlloc "args" $ Just (swiftCast swiftPOP "[Int]")
                           , SwiftAlloc "arg1" $ Just (SwiftTernary (swiftEq (SwiftProj (SwiftIdent "args") "count") (int 2))
                                                                    (arg 1) 
                                                                    (int 0))
                           , SwiftApp (SwiftProj (SwiftIdent "funcWrapper") "fun") [arg 0, SwiftIdent "arg1"]
                        ]
                )
              ]
              where
                arg n = SwiftIndex (SwiftIdent "args") (SwiftNum $ SwiftInt n)
                int n = SwiftNum $ SwiftInt n

      invokeMain :: T.Text
      invokeMain = compileSwift $ SwiftApp (SwiftIdent "main") []

optimizeConstructors :: [Swift] -> ([Swift], [Swift])
optimizeConstructors swift =
    let (swift', cons) = runState (traverse optimizeConstructor' swift) M.empty in
        (map (allocCon . snd) (M.toList cons), swift')
  where
    allocCon :: (String, Swift) -> Swift
    allocCon (name, con) = SwiftAlloc name (Just con)

    newConstructor :: Int -> String
    newConstructor n = "i$CON$" ++ show n

    optimizeConstructor' :: Swift -> State (M.Map Int (String, Swift)) Swift
    optimizeConstructor' swift@(SwiftNew "i$CON" [ SwiftNum (SwiftInt tag)
                                           , SwiftArray []
                                           , a
                                           , e
                                           ]) = do
      s <- get
      case M.lookup tag s of
           Just (i, c) -> return $ SwiftIdent i
           Nothing     -> do let n = newConstructor tag
                             put $ M.insert tag (n, swift) s
                             return $ SwiftIdent n

    optimizeConstructor' (SwiftSeq seq) =
      SwiftSeq <$> traverse optimizeConstructor' seq

    optimizeConstructor' (SwiftSwitch reg cond def) = do
      cond' <- traverse (runKleisli $ arr id *** Kleisli optimizeConstructor') cond
      def'  <- traverse optimizeConstructor' def
      return $ SwiftSwitch reg cond' def'

    optimizeConstructor' (SwiftCond cond) =
      SwiftCond <$> traverse (runKleisli $ arr id *** Kleisli optimizeConstructor') cond

    optimizeConstructor' (SwiftAlloc fun (Just (SwiftFunction args body))) = do
      body' <- optimizeConstructor' body
      return $ SwiftAlloc fun (Just (SwiftFunction args body'))

    optimizeConstructor' (SwiftAssign lhs rhs) = do
      lhs' <- optimizeConstructor' lhs
      rhs' <- optimizeConstructor' rhs
      return $ SwiftAssign lhs' rhs'

    optimizeConstructor' swift = return swift

collectSplitFunctions :: (Swift, [(Int,Swift)]) -> [Swift]
collectSplitFunctions (fun, splits) = map generateSplitFunction splits ++ [fun]
  where
    generateSplitFunction :: (Int,Swift) -> Swift
    generateSplitFunction (depth, SwiftAlloc name fun) =
      SwiftAlloc (name ++ "$" ++ show depth) fun

splitFunction :: Swift -> RWS () [(Int,Swift)] Int Swift
splitFunction (SwiftAlloc name (Just (SwiftFunction args body@(SwiftSeq _)))) = do
  body' <- splitSequence body
  return $ SwiftAlloc name (Just (SwiftFunction args body'))
    where
      splitCondition :: Swift -> RWS () [(Int,Swift)] Int Swift
      splitCondition swift
        | SwiftCond branches <- swift =
            SwiftCond <$> processBranches branches
        | SwiftSwitch cond branches def <- swift =
            SwiftSwitch cond <$> (processBranches branches) <*> (traverse splitSequence def)
        | otherwise = return swift
        where
          processBranches :: [(Swift,Swift)] -> RWS () [(Int,Swift)] Int [(Swift,Swift)]
          processBranches =
            traverse (runKleisli (arr id *** Kleisli splitSequence))

      splitSequence :: Swift -> RWS () [(Int, Swift)] Int Swift
      splitSequence swift@(SwiftSeq seq) =
        let (pre,post) = break isCall seq in
            case post of
                 []                    -> SwiftSeq <$> traverse splitCondition seq
                 [swift@(SwiftCond _)]       -> splitCondition swift
                 [swift@(SwiftSwitch {})] -> splitCondition swift
                 [_]                   -> return swift
                 (call:rest) -> do
                   depth <- get
                   put (depth + 1)
                   new <- splitFunction (newFun rest)
                   tell [(depth, new)]
                   return $ SwiftSeq (pre ++ (newCall depth : [call]))

      splitSequence swift = return swift

      isCall :: Swift -> Bool
      isCall (SwiftApp (SwiftIdent "i$CALL") _) = True
      isCall _                            = False

      newCall :: Int -> Swift
      newCall depth =
        SwiftApp (SwiftIdent "i$CALL") [ SwiftIdent $ name ++ "$" ++ show depth
                                 , SwiftArray [swiftOLDBASE, swiftMYOLDBASE]
                                 ]

      newFun :: [Swift] -> Swift
      newFun seq =
        SwiftAlloc name (Just $ SwiftFunction ["oldbase:Int", "var myoldbase:Int"] (SwiftSeq seq))

splitFunction swift = return swift

translateDecl :: CompileInfo -> (Name, [BC]) -> [Swift]
translateDecl info (name@(MN 0 fun), bc)
  | txt "APPLY" == fun =
         allocCaseFunctions (snd body)
      ++ [ SwiftAlloc (
               translateName name
           ) (Just $ SwiftFunction ["oldbase:Int","var myoldbase:Int"] (
               SwiftSeq $ SwiftAlloc "myoldbase:Int" Nothing : map (translateBC info) (fst body) ++ [
                 SwiftCond [ ( (translateReg $ caseReg (snd body)) `swiftInstanceOf` "i$CON" `swiftAnd` (SwiftProj regAsCon "app")
                          , SwiftApp (SwiftProj regAsCon "app!") [swiftOLDBASE, swiftMYOLDBASE]
                          )
                          , ( SwiftNoop
                            , SwiftSeq $ map (translateBC info) (defaultCase (snd body))
                            )
                        ]
               ]
             )
           )
         ]             

  | txt "EVAL" == fun =
         allocCaseFunctions (snd body)
      ++ [ SwiftAlloc (
               translateName name
           ) (Just $ SwiftFunction ["oldbase:Int","var myoldbase:Int"] (
               SwiftSeq $ SwiftAlloc "myoldbase:Int" Nothing : map (translateBC info) (fst body) ++ [
                 SwiftCond [ ( (translateReg $ caseReg (snd body)) `swiftInstanceOf` "i$CON" `swiftAnd` (SwiftProj regAsCon "ev")
                          , SwiftApp (SwiftProj regAsCon "ev!") [swiftOLDBASE, swiftMYOLDBASE]
                          )
                          , ( SwiftNoop
                            , SwiftSeq $ map (translateBC info) (defaultCase (snd body))
                            )
                        ]
               ]
             )
           )
         ]
  where
    body :: ([BC], [BC])
    body = break isCase bc

    isCase :: BC -> Bool
    isCase bc
      | CASE {} <- bc = True
      | otherwise          = False

    defaultCase :: [BC] -> [BC]
    defaultCase ((CASE _ _ _ (Just d)):_) = d

    caseReg :: [BC] -> Reg
    caseReg ((CASE _ r _ _):_) = r

    allocCaseFunctions :: [BC] -> [Swift]
    allocCaseFunctions ((CASE _ _ c _):_) = splitBranches c
    allocCaseFunctions _                  = []

    splitBranches :: [(Int, [BC])] -> [Swift]
    splitBranches = map prepBranch

    prepBranch :: (Int, [BC]) -> Swift
    prepBranch (tag, code) =
      SwiftAlloc (
        translateName name ++ "$" ++ show tag
      ) (Just $ SwiftFunction ["oldbase:Int", "var myoldbase:Int"] (
          SwiftSeq $ map (translateBC info) code
        )
      )
    
    regAsCon = swiftCast (translateReg $ caseReg (snd body)) "i$CON"
    

translateDecl info (name, bc) =
  [ SwiftAlloc (
       translateName name
     ) (Just $ SwiftFunction ["oldbase:Int","var myoldbase:Int"] (
         SwiftSeq $ SwiftAlloc "myoldbase:Int" Nothing : map (translateBC info)bc
       )
     )
  ]


translateReg :: Reg -> Swift
translateReg reg
  | RVal <- reg = swiftRET
  | Tmp  <- reg = SwiftRaw "//TMPREG"
  | L n  <- reg = swiftLOC n
  | T n  <- reg = swiftTOP n

translateConstant :: Const -> Swift
translateConstant (I i)                    = SwiftNum (SwiftInt i)
translateConstant (Fl f)                   = SwiftNum (SwiftFloat f)
translateConstant (Ch c)                   = SwiftString $ translateChar c
translateConstant (Str s)                  = SwiftString $ concatMap translateChar s
translateConstant (AType (ATInt ITNative)) = SwiftType SwiftIntTy
translateConstant StrType                  = SwiftType SwiftStringTy
translateConstant (AType (ATInt ITBig))    = SwiftType SwiftIntegerTy
translateConstant (AType ATFloat)          = SwiftType SwiftFloatTy
translateConstant (AType (ATInt ITChar))   = SwiftType SwiftCharTy
translateConstant PtrType                  = SwiftType SwiftPtrTy
translateConstant Forgot                   = SwiftType SwiftForgotTy
translateConstant (BI 0)                   = SwiftNum (SwiftInteger SwiftBigZero)
translateConstant (BI 1)                   = SwiftNum (SwiftInteger SwiftBigOne)
translateConstant (BI i)                   = swiftBigInt (SwiftString $ show i)
translateConstant (B8 b)                   = SwiftWord (SwiftWord8 b)
translateConstant (B16 b)                  = SwiftWord (SwiftWord16 b)
translateConstant (B32 b)                  = SwiftWord (SwiftWord32 b)
translateConstant (B64 b)                  = SwiftWord (SwiftWord64 b)
translateConstant c =
  SwiftError $ "Unimplemented Constant: " ++ show c


translateChar :: Char -> String
translateChar ch
  | '\a'   <- ch       = "\\u0007"
  | '\b'   <- ch       = "\\b"
  | '\f'   <- ch       = "\\f"
  | '\n'   <- ch       = "\\n"
  | '\r'   <- ch       = "\\r"
  | '\t'   <- ch       = "\\t"
  | '\v'   <- ch       = "\\v"
  | '\SO'  <- ch       = "\\u000E"
  | '\DEL' <- ch       = "\\u007F"
  | '\\'   <- ch       = "\\\\"
  | '\"'   <- ch       = "\\\""
  | '\''   <- ch       = "\\\'"
  | ch `elem` asciiTab = "\\u00" ++ fill (showHex (ord ch) "")
  | otherwise          = [ch]
  where
    fill :: String -> String
    fill s = if length s == 1
                then '0' : s
                else s

    asciiTab =
      ['\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK', '\BEL',
       '\BS',  '\HT',  '\LF',  '\VT',  '\FF',  '\CR',  '\SO',  '\SI',
       '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB',
       '\CAN', '\EM',  '\SUB', '\ESC', '\FS',  '\GS',  '\RS',  '\US']

translateName :: Name -> String
translateName n = "_idris_" ++ concatMap cchar (showCG n)
  where cchar x | isAlphaNum x = [x]
                | otherwise    = "_" ++ show (fromEnum x) ++ "_"

swiftASSIGN :: CompileInfo -> Reg -> Reg -> Swift
swiftASSIGN _ r1 r2 = SwiftAssign (translateReg r1) (translateReg r2)

swiftASSIGNCONST :: CompileInfo -> Reg -> Const -> Swift
swiftASSIGNCONST _ r c = SwiftAssign (translateReg r) (translateConstant c)

swiftCALL :: CompileInfo -> Name -> Swift
swiftCALL _ n =
  SwiftApp (
    SwiftIdent "i$CALL"
  ) [SwiftIdent (translateName n), SwiftArray [swiftMYOLDBASE]]

swiftTAILCALL :: CompileInfo -> Name -> Swift
swiftTAILCALL _ n =
  SwiftApp (
    SwiftIdent "i$CALL"
  ) [SwiftIdent (translateName n), SwiftArray [swiftOLDBASE]]

swiftFOREIGN :: CompileInfo -> Reg -> String -> [(FType, Reg)] -> Swift
swiftFOREIGN _ reg n args
  | n == "putStr"
  , [(FString, arg)] <- args =
      SwiftAssign (
        translateReg reg
      ) (
        SwiftApp (SwiftIdent "i$putStr") [translateReg arg]
      )

  | n == "isNull"
  , [(FPtr, arg)] <- args =
      SwiftAssign (
        translateReg reg
      ) (
        SwiftBinOp "==" (translateReg arg) SwiftNull
      )

  | n == "idris_eqPtr"
  , [(_, lhs),(_, rhs)] <- args =
      SwiftAssign (
        translateReg reg
      ) (
        SwiftBinOp "==" (translateReg lhs) (translateReg rhs)
      )
  | otherwise =
     SwiftAssign (
       translateReg reg
     ) (
       SwiftFFI n (map generateWrapper args)
     )
    where
      generateWrapper :: (FType, Reg) -> Swift
      generateWrapper (ty, reg)
        | FFunction   <- ty =
            SwiftApp (SwiftIdent "i$ffiWrap") [ translateReg reg
                                        , SwiftIdent "oldbase"
                                        , SwiftIdent "myoldbase"
                                        ]
        | FFunctionIO <- ty =
            SwiftApp (SwiftIdent "i$ffiWrap") [ translateReg reg
                                        , SwiftIdent "oldbase"
                                        , SwiftIdent "myoldbase"
                                        ]

      generateWrapper (_, reg) =
        translateReg reg

swiftREBASE :: CompileInfo -> Swift
swiftREBASE _ = SwiftAssign swiftSTACKBASE swiftOLDBASE

swiftSTOREOLD :: CompileInfo ->Swift
swiftSTOREOLD _ = SwiftAssign swiftMYOLDBASE swiftSTACKBASE

swiftADDTOP :: CompileInfo -> Int -> Swift
swiftADDTOP info n
  | 0 <- n    = SwiftNoop
  | otherwise =
      -- SwiftBinOp "+=" swiftSTACKTOP (SwiftNum (SwiftInt n))
      SwiftBinOp ";" (SwiftBinOp "+=" swiftSTACKTOP (SwiftNum (SwiftInt n))) (fillStack n)
    where
      fillStack :: Int -> Swift
      fillStack n = SwiftBinOp "+=" swiftSTACK (SwiftArray $ replicate (n+1) SwiftNull)

swiftTOPBASE :: CompileInfo -> Int -> Swift
swiftTOPBASE _ 0  = SwiftAssign swiftSTACKTOP swiftSTACKBASE
swiftTOPBASE _ n  = SwiftAssign swiftSTACKTOP (SwiftBinOp "+" swiftSTACKBASE (SwiftNum (SwiftInt n)))


swiftBASETOP :: CompileInfo -> Int -> Swift
swiftBASETOP _ 0 = SwiftAssign swiftSTACKBASE swiftSTACKTOP
swiftBASETOP _ n = SwiftAssign swiftSTACKBASE (SwiftBinOp "+" swiftSTACKTOP (SwiftNum (SwiftInt n)))

swiftNULL :: CompileInfo -> Reg -> Swift
swiftNULL _ r = SwiftAssign (translateReg r) SwiftNull

swiftERROR :: CompileInfo -> String -> Swift
swiftERROR _ = SwiftError

swiftSLIDE :: CompileInfo -> Int -> Swift
swiftSLIDE _ 1 = SwiftAssign (swiftLOC 0) (swiftTOP 0)
swiftSLIDE _ n = SwiftApp (SwiftIdent "i$SLIDE") [SwiftNum (SwiftInt n)]

swiftMKCON :: CompileInfo -> Reg -> Int -> [Reg] -> Swift
swiftMKCON info r t rs =
  SwiftAssign (translateReg r) (
    SwiftNew "i$CON" [ SwiftNum (SwiftInt t)
                  , SwiftArray (map translateReg rs)
                  , if t `elem` compileInfoApplyCases info
                       then SwiftIdent $ translateName (sMN 0 "APPLY") ++ "$" ++ show t
                       else SwiftNull
                  , if t `elem` compileInfoEvalCases info
                       then SwiftIdent $ translateName (sMN 0 "EVAL") ++ "$" ++ show t
                       else SwiftNull
                  ]
  )

swiftCASE :: CompileInfo -> Bool -> Reg -> [(Int, [BC])] -> Maybe [BC] -> Swift
swiftCASE info safe reg cases def =
  SwiftSwitch (tag safe $ translateReg reg) (
    map ((SwiftNum . SwiftInt) *** prepBranch) cases
  ) (fmap prepBranch def)
    where
      tag :: Bool -> Swift -> Swift
      tag True  = swiftCTAG
      tag False = swiftTAG

      prepBranch :: [BC] -> Swift
      prepBranch bc = SwiftSeq $ map (translateBC info) bc

      swiftTAG :: Swift -> Swift
      swiftTAG swift =
        (SwiftTernary (swift `swiftInstanceOf` "i$CON") (
          SwiftProj swift "tag"
        ) (SwiftNum (SwiftInt $ negate 1)))

      swiftCTAG :: Swift -> Swift
      swiftCTAG swift = SwiftProj swift "tag"


swiftCONSTCASE :: CompileInfo -> Reg -> [(Const, [BC])] -> Maybe [BC] -> Swift
swiftCONSTCASE info reg cases def =
  SwiftCond $ (
    map (swiftEq (translateReg reg) . translateConstant *** prepBranch) cases
  ) ++ (maybe [] ((:[]) . ((,) SwiftNoop) . prepBranch) def)
    where
      prepBranch :: [BC] -> Swift
      prepBranch bc = SwiftSeq $ map (translateBC info) bc

swiftPROJECT :: CompileInfo -> Reg -> Int -> Int -> Swift
swiftPROJECT _ reg loc 0  = SwiftNoop
swiftPROJECT _ reg loc 1  =
  SwiftAssign (swiftLOC loc) (
    SwiftIndex (
      SwiftProj (swiftCast (translateReg reg) "i$CON") "args"
    ) (
      SwiftNum (SwiftInt 0)
    )
  )
swiftPROJECT _ reg loc ar =
  SwiftApp (SwiftIdent "i$PROJECT") [ translateReg reg
                              , SwiftNum (SwiftInt loc)
                              , SwiftNum (SwiftInt ar)
                              ]

swiftOP :: CompileInfo -> Reg -> PrimFn -> [Reg] -> Swift
swiftOP _ reg op args = SwiftAssign (translateReg reg) swiftOP'
  where
    swiftOP' :: Swift
    swiftOP'
      | LNoOp <- op = translateReg (last args)

      | (LZExt (ITFixed IT8) ITNative)  <- op = swiftUnPackBits $ translateReg (last args)
      | (LZExt (ITFixed IT16) ITNative) <- op = swiftUnPackBits $ translateReg (last args)
      | (LZExt (ITFixed IT32) ITNative) <- op = swiftUnPackBits $ translateReg (last args)

      | (LZExt _ ITBig)        <- op = swiftBigInt $ SwiftApp  (SwiftIdent "String") [translateReg (last args)]
      | (LPlus (ATInt ITBig))  <- op
      , (lhs:rhs:_)            <- args = invokeMeth lhs "add" [rhs]
      | (LMinus (ATInt ITBig)) <- op
      , (lhs:rhs:_)            <- args = invokeMeth lhs "subtract" [rhs]
      | (LTimes (ATInt ITBig)) <- op
      , (lhs:rhs:_)            <- args = invokeMeth lhs "multiply" [rhs]
      | (LSDiv (ATInt ITBig))  <- op
      , (lhs:rhs:_)            <- args = invokeMeth lhs "divide" [rhs]
      | (LSRem (ATInt ITBig))  <- op
      , (lhs:rhs:_)            <- args = invokeMeth lhs "mod" [rhs]
      | (LEq (ATInt ITBig))    <- op
      , (lhs:rhs:_)            <- args = invokeMeth lhs "equals" [rhs]
      | (LSLt (ATInt ITBig))   <- op
      , (lhs:rhs:_)            <- args = invokeMeth lhs "lesser" [rhs]
      | (LSLe (ATInt ITBig))   <- op
      , (lhs:rhs:_)            <- args = invokeMeth lhs "lesserOrEquals" [rhs]
      | (LSGt (ATInt ITBig))   <- op
      , (lhs:rhs:_)            <- args = invokeMeth lhs "greater" [rhs]
      | (LSGe (ATInt ITBig))   <- op
      , (lhs:rhs:_)            <- args = invokeMeth lhs "greaterOrEquals" [rhs]

      | (LPlus ATFloat)  <- op
      , (lhs:rhs:_)      <- args = translateBinaryOp "+" lhs rhs
      | (LMinus ATFloat) <- op
      , (lhs:rhs:_)      <- args = translateBinaryOp "-" lhs rhs
      | (LTimes ATFloat) <- op
      , (lhs:rhs:_)      <- args = translateBinaryOp "*" lhs rhs
      | (LSDiv ATFloat)  <- op
      , (lhs:rhs:_)      <- args = translateBinaryOp "/" lhs rhs
      | (LEq ATFloat)    <- op
      , (lhs:rhs:_)      <- args = translateBinaryOp "==" lhs rhs
      | (LSLt ATFloat)   <- op
      , (lhs:rhs:_)      <- args = translateBinaryOp "<" lhs rhs
      | (LSLe ATFloat)   <- op
      , (lhs:rhs:_)      <- args = translateBinaryOp "<=" lhs rhs
      | (LSGt ATFloat)   <- op
      , (lhs:rhs:_)      <- args = translateBinaryOp ">" lhs rhs
      | (LSGe ATFloat)   <- op
      , (lhs:rhs:_)      <- args = translateBinaryOp ">=" lhs rhs

      | (LPlus (ATInt ITChar)) <- op
      , (lhs:rhs:_)            <- args =
          swiftCall "i$fromCharCode" [
            SwiftBinOp "+" (
              swiftCall "i$charCode" [translateReg lhs]
            ) (
              swiftCall "i$charCode" [translateReg rhs]
            )
          ]

      | (LTrunc (ITFixed IT16) (ITFixed IT8)) <- op
      , (arg:_)                               <- args =
          swiftPackUBits8 (
            SwiftBinOp "&" (swiftUnPackBits $ translateReg arg) (SwiftNum (SwiftInt 0xFF))
          )

      | (LTrunc (ITFixed IT32) (ITFixed IT16)) <- op
      , (arg:_)                                <- args =
          swiftPackUBits16 (
            SwiftBinOp "&" (swiftUnPackBits $ translateReg arg) (SwiftNum (SwiftInt 0xFFFF))
          )

      | (LTrunc (ITFixed IT64) (ITFixed IT32)) <- op
      , (arg:_)                                <- args =
          swiftPackUBits32 (
            swiftMeth (swiftMeth (translateReg arg) "and" [
              swiftBigInt (SwiftString $ show 0xFFFFFFFF)
            ]) "intValue" []
          )

      | (LTrunc ITBig (ITFixed IT64)) <- op
      , (arg:_)                       <- args =
          swiftMeth (translateReg arg) "and" [
            swiftBigInt (SwiftString $ show 0xFFFFFFFFFFFFFFFF)
          ]

      | (LLSHR (ITFixed IT8)) <- op
      , (lhs:rhs:_)           <- args =
          swiftPackUBits8 (
            SwiftBinOp ">>" (swiftUnPackBits $ translateReg lhs) (swiftUnPackBits $ translateReg rhs)
          )

      | (LLSHR (ITFixed IT16)) <- op
      , (lhs:rhs:_)            <- args =
          swiftPackUBits16 (
            SwiftBinOp ">>" (swiftUnPackBits $ translateReg lhs) (swiftUnPackBits $ translateReg rhs)
          )

      | (LLSHR (ITFixed IT32)) <- op
      , (lhs:rhs:_)            <- args =
          swiftPackUBits32  (
            SwiftBinOp ">>" (swiftUnPackBits $ translateReg lhs) (swiftUnPackBits $ translateReg rhs)
          )

      | (LLSHR (ITFixed IT64)) <- op
      , (lhs:rhs:_)            <- args =
          swiftMeth (translateReg lhs) "shiftRight" [translateReg rhs]

      | (LSHL (ITFixed IT8)) <- op
      , (lhs:rhs:_)          <- args =
          swiftPackUBits8 (
            SwiftBinOp "<<" (swiftUnPackBits $ translateReg lhs) (swiftUnPackBits $ translateReg rhs)
          )

      | (LSHL (ITFixed IT16)) <- op
      , (lhs:rhs:_)           <- args =
          swiftPackUBits16 (
            SwiftBinOp "<<" (swiftUnPackBits $ translateReg lhs) (swiftUnPackBits $ translateReg rhs)
          )

      | (LSHL (ITFixed IT32)) <- op
      , (lhs:rhs:_)           <- args =
          swiftPackUBits32  (
            SwiftBinOp "<<" (swiftUnPackBits $ translateReg lhs) (swiftUnPackBits $ translateReg rhs)
          )

      | (LSHL (ITFixed IT64)) <- op
      , (lhs:rhs:_)           <- args =
          swiftMeth (swiftMeth (translateReg lhs) "shiftLeft" [translateReg rhs]) "and" [
            swiftBigInt (SwiftString $ show 0xFFFFFFFFFFFFFFFF)
          ]

      | (LAnd (ITFixed IT8)) <- op
      , (lhs:rhs:_)          <- args =
          swiftPackUBits8 (
            SwiftBinOp "&" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LAnd (ITFixed IT16)) <- op
      , (lhs:rhs:_)           <- args =
          swiftPackUBits16 (
            SwiftBinOp "&" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LAnd (ITFixed IT32)) <- op
      , (lhs:rhs:_)           <- args =
          swiftPackUBits32 (
            SwiftBinOp "&" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LAnd (ITFixed IT64)) <- op
      , (lhs:rhs:_)           <- args =
          swiftMeth (translateReg lhs) "and" [translateReg rhs]

      | (LOr (ITFixed IT8)) <- op
      , (lhs:rhs:_)         <- args =
          swiftPackUBits8 (
            SwiftBinOp "|" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LOr (ITFixed IT16)) <- op
      , (lhs:rhs:_)          <- args =
          swiftPackUBits16 (
            SwiftBinOp "|" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LOr (ITFixed IT32)) <- op
      , (lhs:rhs:_)          <- args =
          swiftPackUBits32 (
            SwiftBinOp "|" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LOr (ITFixed IT64)) <- op
      , (lhs:rhs:_)          <- args =
          swiftMeth (translateReg lhs) "or" [translateReg rhs]

      | (LXOr (ITFixed IT8)) <- op
      , (lhs:rhs:_)          <- args =
          swiftPackUBits8 (
            SwiftBinOp "^" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LXOr (ITFixed IT16)) <- op
      , (lhs:rhs:_)           <- args =
          swiftPackUBits16 (
            SwiftBinOp "^" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LXOr (ITFixed IT32)) <- op
      , (lhs:rhs:_)           <- args =
          swiftPackUBits32 (
            SwiftBinOp "^" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LXOr (ITFixed IT64)) <- op
      , (lhs:rhs:_)           <- args =
          swiftMeth (translateReg lhs) "xor" [translateReg rhs]

      | (LPlus (ATInt (ITFixed IT8))) <- op
      , (lhs:rhs:_)                   <- args =
          swiftPackUBits8 (
            SwiftBinOp "+" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LPlus (ATInt (ITFixed IT16))) <- op
      , (lhs:rhs:_)                    <- args =
          swiftPackUBits16 (
            SwiftBinOp "+" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LPlus (ATInt (ITFixed IT32))) <- op
      , (lhs:rhs:_)                    <- args =
          swiftPackUBits32 (
            SwiftBinOp "+" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LPlus (ATInt (ITFixed IT64))) <- op
      , (lhs:rhs:_)                    <- args =
          swiftMeth (swiftMeth (translateReg lhs) "add" [translateReg rhs]) "and" [
            swiftBigInt (SwiftString $ show 0xFFFFFFFFFFFFFFFF)
          ]

      | (LMinus (ATInt (ITFixed IT8))) <- op
      , (lhs:rhs:_)                    <- args =
          swiftPackUBits8 (
            SwiftBinOp "-" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LMinus (ATInt (ITFixed IT16))) <- op
      , (lhs:rhs:_)                     <- args =
          swiftPackUBits16 (
            SwiftBinOp "-" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LMinus (ATInt (ITFixed IT32))) <- op
      , (lhs:rhs:_)                     <- args =
          swiftPackUBits32 (
            SwiftBinOp "-" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LMinus (ATInt (ITFixed IT64))) <- op
      , (lhs:rhs:_)                     <- args =
          swiftMeth (swiftMeth (translateReg lhs) "subtract" [translateReg rhs]) "and" [
            swiftBigInt (SwiftString $ show 0xFFFFFFFFFFFFFFFF)
          ]

      | (LTimes (ATInt (ITFixed IT8))) <- op
      , (lhs:rhs:_)                    <- args =
          swiftPackUBits8 (
            SwiftBinOp "*" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LTimes (ATInt (ITFixed IT16))) <- op
      , (lhs:rhs:_)                     <- args =
          swiftPackUBits16 (
            SwiftBinOp "*" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LTimes (ATInt (ITFixed IT32))) <- op
      , (lhs:rhs:_)                     <- args =
          swiftPackUBits32 (
            SwiftBinOp "*" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LTimes (ATInt (ITFixed IT64))) <- op
      , (lhs:rhs:_)                     <- args =
          swiftMeth (swiftMeth (translateReg lhs) "multiply" [translateReg rhs]) "and" [
            swiftBigInt (SwiftString $ show 0xFFFFFFFFFFFFFFFF)
          ]

      | (LEq (ATInt (ITFixed IT8))) <- op
      , (lhs:rhs:_)                 <- args =
          swiftPackUBits8 (
            SwiftBinOp "==" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LEq (ATInt (ITFixed IT16))) <- op
      , (lhs:rhs:_)                  <- args =
          swiftPackUBits16 (
            SwiftBinOp "==" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LEq (ATInt (ITFixed IT32))) <- op
      , (lhs:rhs:_)                  <- args =
          swiftPackUBits32 (
            SwiftBinOp "==" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LEq (ATInt (ITFixed IT64))) <- op
      , (lhs:rhs:_)                   <- args =
          swiftMeth (swiftMeth (translateReg lhs) "equals" [translateReg rhs]) "and" [
            swiftBigInt (SwiftString $ show 0xFFFFFFFFFFFFFFFF)
          ]

      | (LLt (ITFixed IT8)) <- op
      , (lhs:rhs:_)         <- args =
          swiftPackUBits8 (
            SwiftBinOp "<" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LLt (ITFixed IT16)) <- op
      , (lhs:rhs:_)          <- args =
          swiftPackUBits16 (
            SwiftBinOp "<" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LLt (ITFixed IT32)) <- op
      , (lhs:rhs:_)          <- args =
          swiftPackUBits32 (
            SwiftBinOp "<" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LLt (ITFixed IT64)) <- op
      , (lhs:rhs:_)          <- args = invokeMeth lhs "lesser" [rhs]

      | (LLe (ITFixed IT8)) <- op
      , (lhs:rhs:_)         <- args =
          swiftPackUBits8 (
            SwiftBinOp "<=" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LLe (ITFixed IT16)) <- op
      , (lhs:rhs:_)          <- args =
          swiftPackUBits16 (
            SwiftBinOp "<=" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LLe (ITFixed IT32)) <- op
      , (lhs:rhs:_)          <- args =
          swiftPackUBits32 (
            SwiftBinOp "<=" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LLe (ITFixed IT64)) <- op
      , (lhs:rhs:_)          <- args = invokeMeth lhs "lesserOrEquals" [rhs]

      | (LGt (ITFixed IT8)) <- op
      , (lhs:rhs:_)         <- args =
          swiftPackUBits8 (
            SwiftBinOp ">" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LGt (ITFixed IT16)) <- op
      , (lhs:rhs:_)          <- args =
          swiftPackUBits16 (
            SwiftBinOp ">" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )
      | (LGt (ITFixed IT32)) <- op
      , (lhs:rhs:_)          <- args =
          swiftPackUBits32 (
            SwiftBinOp ">" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LGt (ITFixed IT64)) <- op
      , (lhs:rhs:_)          <- args = invokeMeth lhs "greater" [rhs]

      | (LGe (ITFixed IT8)) <- op
      , (lhs:rhs:_)         <- args =
          swiftPackUBits8 (
            SwiftBinOp ">=" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LGe (ITFixed IT16)) <- op
      , (lhs:rhs:_)          <- args =
          swiftPackUBits16 (
            SwiftBinOp ">=" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )
      | (LGe (ITFixed IT32)) <- op
      , (lhs:rhs:_)          <- args =
          swiftPackUBits32 (
            SwiftBinOp ">=" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LGe (ITFixed IT64)) <- op
      , (lhs:rhs:_)          <- args = invokeMeth lhs "greaterOrEquals" [rhs]

      | (LUDiv (ITFixed IT8)) <- op
      , (lhs:rhs:_)           <- args =
          swiftPackUBits8 (
            SwiftBinOp "/" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LUDiv (ITFixed IT16)) <- op
      , (lhs:rhs:_)            <- args =
          swiftPackUBits16 (
            SwiftBinOp "/" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LUDiv (ITFixed IT32)) <- op
      , (lhs:rhs:_)            <- args =
          swiftPackUBits32 (
            SwiftBinOp "/" (swiftUnPackBits (translateReg lhs)) (swiftUnPackBits (translateReg rhs))
          )

      | (LUDiv (ITFixed IT64)) <- op
      , (lhs:rhs:_)            <- args = invokeMeth lhs "divide" [rhs]

      | (LSDiv (ATInt (ITFixed IT8))) <- op
      , (lhs:rhs:_)                   <- args =
          swiftPackSBits8 (
            SwiftBinOp "/" (
              swiftUnPackBits $ swiftPackSBits8 $ swiftUnPackBits (translateReg lhs)
            ) (
              swiftUnPackBits $ swiftPackSBits8 $ swiftUnPackBits (translateReg rhs)
            )
          )

      | (LSDiv (ATInt (ITFixed IT16))) <- op
      , (lhs:rhs:_)                    <- args =
          swiftPackSBits16 (
            SwiftBinOp "/" (
              swiftUnPackBits $ swiftPackSBits16 $ swiftUnPackBits (translateReg lhs)
            ) (
              swiftUnPackBits $ swiftPackSBits16 $ swiftUnPackBits (translateReg rhs)
            )
          )

      | (LSDiv (ATInt (ITFixed IT32))) <- op
      , (lhs:rhs:_)                    <- args =
          swiftPackSBits32 (
            SwiftBinOp "/" (
              swiftUnPackBits $ swiftPackSBits32 $ swiftUnPackBits (translateReg lhs)
            ) (
              swiftUnPackBits $ swiftPackSBits32 $ swiftUnPackBits (translateReg rhs)
            )
          )

      | (LSDiv (ATInt (ITFixed IT64))) <- op
      , (lhs:rhs:_)                    <- args = invokeMeth lhs "divide" [rhs]

      | (LSRem (ATInt (ITFixed IT8))) <- op
      , (lhs:rhs:_)                   <- args =
          swiftPackSBits8 (
            SwiftBinOp "%" (
              swiftUnPackBits $ swiftPackSBits8 $ swiftUnPackBits (translateReg lhs)
            ) (
              swiftUnPackBits $ swiftPackSBits8 $ swiftUnPackBits (translateReg rhs)
            )
          )

      | (LSRem (ATInt (ITFixed IT16))) <- op
      , (lhs:rhs:_)                    <- args =
          swiftPackSBits16 (
            SwiftBinOp "%" (
              swiftUnPackBits $ swiftPackSBits16 $ swiftUnPackBits (translateReg lhs)
            ) (
              swiftUnPackBits $ swiftPackSBits16 $ swiftUnPackBits (translateReg rhs)
            )
          )

      | (LSRem (ATInt (ITFixed IT32))) <- op
      , (lhs:rhs:_)                    <- args =
          swiftPackSBits32 (
            SwiftBinOp "%" (
              swiftUnPackBits $ swiftPackSBits32 $ swiftUnPackBits (translateReg lhs)
            ) (
              swiftUnPackBits $ swiftPackSBits32 $ swiftUnPackBits (translateReg rhs)
            )
          )

      | (LSRem (ATInt (ITFixed IT64))) <- op
      , (lhs:rhs:_)                    <- args = invokeMeth lhs "mod" [rhs]

      | (LCompl (ITFixed IT8)) <- op
      , (arg:_)                <- args =
          swiftPackSBits8 $ SwiftPreOp "~" $ swiftUnPackBits (translateReg arg)

      | (LCompl (ITFixed IT16)) <- op
      , (arg:_)                 <- args =
          swiftPackSBits16 $ SwiftPreOp "~" $ swiftUnPackBits (translateReg arg)

      | (LCompl (ITFixed IT32)) <- op
      , (arg:_)                 <- args =
          swiftPackSBits32 $ SwiftPreOp "~" $ swiftUnPackBits (translateReg arg)

      | (LCompl (ITFixed IT64)) <- op
      , (arg:_)     <- args =
          invokeMeth arg "not" []

      | (LPlus _)   <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "+" lhs rhs
      | (LMinus _)  <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "-" lhs rhs
      | (LTimes _)  <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "*" lhs rhs
      | (LSDiv _)   <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "/" lhs rhs
      | (LSRem _)   <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "%" lhs rhs
      | (LEq _)     <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "==" lhs rhs
      | (LSLt _)    <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "<" lhs rhs
      | (LSLe _)    <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "<=" lhs rhs
      | (LSGt _)    <- op
      , (lhs:rhs:_) <- args = translateBinaryOp ">" lhs rhs
      | (LSGe _)    <- op
      , (lhs:rhs:_) <- args = translateBinaryOp ">=" lhs rhs
      | (LAnd _)    <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "&" lhs rhs
      | (LOr _)     <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "|" lhs rhs
      | (LXOr _)    <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "^" lhs rhs
      | (LSHL _)    <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "<<" rhs lhs
      | (LASHR _)   <- op
      , (lhs:rhs:_) <- args = translateBinaryOp ">>" rhs lhs
      | (LCompl _)  <- op
      , (arg:_)     <- args = SwiftPreOp "~" (translateReg arg)

      | LStrConcat  <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "+" lhs rhs
      | LStrEq      <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "==" lhs rhs
      | LStrLt      <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "<" lhs rhs
      | LStrLen     <- op
      , (arg:_)     <- args = SwiftProj (translateReg arg) "length"
      | (LStrInt ITNative)      <- op
      , (arg:_)                 <- args = swiftCall "parseInt" [translateReg arg]
      | (LIntStr ITNative)      <- op
      , (arg:_)                 <- args = swiftCall "String" [translateReg arg]
      | (LSExt ITNative ITBig)  <- op
      , (arg:_)                 <- args = swiftBigInt $ swiftCall "String" [translateReg arg]
      | (LTrunc ITBig ITNative) <- op
      , (arg:_)                 <- args = swiftMeth (translateReg arg) "intValue" []
      | (LIntStr ITBig)         <- op
      , (arg:_)                 <- args = swiftMeth (translateReg arg) "toString" []
      | (LStrInt ITBig)         <- op
      , (arg:_)                 <- args = swiftBigInt $ translateReg arg
      | LFloatStr               <- op
      , (arg:_)                 <- args = swiftCall "String" [translateReg arg]
      | LStrFloat               <- op
      , (arg:_)                 <- args = swiftCall "parseFloat" [translateReg arg]
      | (LIntFloat ITNative)    <- op
      , (arg:_)                 <- args = translateReg arg
      | (LFloatInt ITNative)    <- op
      , (arg:_)                 <- args = translateReg arg
      | (LChInt ITNative)       <- op
      , (arg:_)                 <- args = swiftCall "i$charCode" [translateReg arg]
      | (LIntCh ITNative)       <- op
      , (arg:_)                 <- args = swiftCall "i$fromCharCode" [translateReg arg]

      | LFExp       <- op
      , (arg:_)     <- args = swiftCall "Math.exp" [translateReg arg]
      | LFLog       <- op
      , (arg:_)     <- args = swiftCall "Math.log" [translateReg arg]
      | LFSin       <- op
      , (arg:_)     <- args = swiftCall "Math.sin" [translateReg arg]
      | LFCos       <- op
      , (arg:_)     <- args = swiftCall "Math.cos" [translateReg arg]
      | LFTan       <- op
      , (arg:_)     <- args = swiftCall "Math.tan" [translateReg arg]
      | LFASin      <- op
      , (arg:_)     <- args = swiftCall "Math.asin" [translateReg arg]
      | LFACos      <- op
      , (arg:_)     <- args = swiftCall "Math.acos" [translateReg arg]
      | LFATan      <- op
      , (arg:_)     <- args = swiftCall "Math.atan" [translateReg arg]
      | LFSqrt      <- op
      , (arg:_)     <- args = swiftCall "Math.sqrt" [translateReg arg]
      | LFFloor     <- op
      , (arg:_)     <- args = swiftCall "Math.floor" [translateReg arg]
      | LFCeil      <- op
      , (arg:_)     <- args = swiftCall "Math.ceil" [translateReg arg]

      | LStrCons    <- op
      , (lhs:rhs:_) <- args = invokeMeth lhs "concat" [rhs]
      | LStrHead    <- op
      , (arg:_)     <- args = SwiftIndex (translateReg arg) (SwiftNum (SwiftInt 0))
      | LStrRev     <- op
      , (arg:_)     <- args = SwiftProj (translateReg arg) "split('').reverse().join('')"
      | LStrIndex   <- op
      , (lhs:rhs:_) <- args = SwiftIndex (translateReg lhs) (translateReg rhs)
      | LStrTail    <- op
      , (arg:_)     <- args =
          let v = translateReg arg in
              SwiftApp (SwiftProj v "substr") [
                SwiftNum (SwiftInt 1),
                SwiftBinOp "-" (SwiftProj v "length") (SwiftNum (SwiftInt 1))
              ]

      | LSystemInfo <- op
      , (arg:_) <- args = swiftCall "i$systemInfo"  [translateReg arg]
      | LNullPtr    <- op
      , (_)         <- args = SwiftNull
      | otherwise = SwiftError $ "Not implemented: " ++ show op
        where
          translateBinaryOp :: String -> Reg -> Reg -> Swift
          translateBinaryOp op lhs rhs =
            SwiftBinOp op (translateReg lhs) (translateReg rhs)

          invokeMeth :: Reg -> String -> [Reg] -> Swift
          invokeMeth obj meth args =
            SwiftApp (SwiftProj (translateReg obj) meth) $ map translateReg args


swiftRESERVE :: CompileInfo -> Int -> Swift
swiftRESERVE _ _ = SwiftNoop

swiftSTACK :: Swift
swiftSTACK = SwiftIdent "i$state.valstack"

swiftCALLSTACK :: Swift
swiftCALLSTACK = SwiftIdent "i$state.callstack"

swiftSTACKBASE :: Swift
swiftSTACKBASE = SwiftIdent "i$state.valstack_base"

swiftSTACKTOP :: Swift
swiftSTACKTOP = SwiftIdent "i$state.valstack_top"

swiftOLDBASE :: Swift
swiftOLDBASE = SwiftIdent "oldbase"

swiftMYOLDBASE :: Swift
swiftMYOLDBASE = SwiftIdent "myoldbase"

swiftRET :: Swift
swiftRET = SwiftIdent "i$state.ret"

swiftLOC :: Int -> Swift
swiftLOC 0 = SwiftIndex swiftSTACK swiftSTACKBASE
swiftLOC n = SwiftIndex swiftSTACK (SwiftBinOp "+" swiftSTACKBASE (SwiftNum (SwiftInt n)))

swiftTOP :: Int -> Swift
swiftTOP 0 = SwiftIndex swiftSTACK swiftSTACKTOP
swiftTOP n = SwiftIndex swiftSTACK (SwiftBinOp "+" swiftSTACKTOP (SwiftNum (SwiftInt n)))

swiftPUSH :: [Swift] -> Swift
swiftPUSH args = SwiftApp (SwiftProj swiftCALLSTACK "append") args

swiftPOP :: Swift
swiftPOP = SwiftApp (SwiftProj swiftCALLSTACK "removeLast") []

translateBC :: CompileInfo -> BC -> Swift
translateBC info bc
  | ASSIGN r1 r2          <- bc = swiftASSIGN info r1 r2
  | ASSIGNCONST r c       <- bc = swiftASSIGNCONST info r c
  | UPDATE r1 r2          <- bc = swiftASSIGN info r1 r2
  | ADDTOP n              <- bc = swiftADDTOP info n
  | NULL r                <- bc = swiftNULL info r
  | CALL n                <- bc = swiftCALL info n
  | TAILCALL n            <- bc = swiftTAILCALL info n
  | FOREIGNCALL r _ _ n a <- bc = swiftFOREIGN info r n a
  | TOPBASE n             <- bc = swiftTOPBASE info n
  | BASETOP n             <- bc = swiftBASETOP info n
  | STOREOLD              <- bc = swiftSTOREOLD info
  | SLIDE n               <- bc = swiftSLIDE info n
  | REBASE                <- bc = swiftREBASE info
  | RESERVE n             <- bc = swiftRESERVE info n
  | MKCON r t rs          <- bc = swiftMKCON info r t rs
  | CASE s r c d          <- bc = swiftCASE info s r c d
  | CONSTCASE r c d       <- bc = swiftCONSTCASE info r c d
  | PROJECT r l a         <- bc = swiftPROJECT info r l a
  | OP r o a              <- bc = swiftOP info r o a
  | ERROR e               <- bc = swiftERROR info e
  | otherwise                   = SwiftRaw $ "//" ++ show bc

