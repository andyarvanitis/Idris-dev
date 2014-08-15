{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module IRTS.CodegenCpp (codegenCpp, CppTarget(..)) where

import IRTS.Cpp.AST

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
import Data.Bits
import System.IO
import System.Process
import System.Exit
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


data CppTarget = Cpp deriving Eq

codegenCpp :: CodeGenerator
codegenCpp ci =
  codegenCpp_all (simpleDecls ci)
                 (outputType ci)
                 (outputFile ci)
                 (includes ci)
                 (concatMap mkObj (compileObjs ci))
                 (concatMap mkLib (compileLibs ci) ++
                     concatMap incdir (importDirs ci))
                 (concatMap mkFlag (compilerFlags ci))
                 (debugLevel ci)
    where
      mkObj f = f ++ " "
      mkLib l = "-l" ++ l ++ " "
      mkFlag l = l ++ " "
      incdir i = "-I" ++ i ++ " "

codegenCpp_all :: 
     [(Name, SDecl)] -> -- declarations/definitions
     OutputType ->      -- output type
     FilePath ->        -- output file name
     [FilePath] ->      -- include files
     String ->          -- extra object files
     String ->          -- libraries
     String ->          -- extra compiler flags
     DbgLevel ->        -- debug level
     IO ()

codegenCpp_all definitions outputType filename includes objs libs flags dbg = do
  let bytecode = map toBC definitions
  let info = initCompileInfo bytecode
  let cpp = concatMap (translateDecl info) bytecode
  let full = concatMap processFunction cpp
  let code = deadCodeElim full
  let (cons, opt) = optimizeConstructors code
  let (header, rt) = ("", "")
  
  path       <- (++) <$> getDataDir <*> (pure "/cpprts/")
  idrRuntime <- readFile $ path ++ "runtime.cpp"
                
  let cppout = (  T.pack idrRuntime
                  `T.append` T.pack (headers includes)
                  `T.append` T.concat (map varDecl opt)
                  `T.append` T.pack "\n\n"
                  `T.append` T.concat (map varDecl cons)
                  `T.append` T.pack "\n\n"
                  `T.append` T.concat (map compileCpp opt)
                  `T.append` T.concat (map compileCpp cons)
                  `T.append` main
                  `T.append` invokeMain
               )
  case outputType of
    Raw -> TIO.writeFile filename cppout
    _ -> do (tmpn, tmph) <- tempfile
            hPutStr tmph (T.unpack cppout)
            hFlush tmph
            hClose tmph
            comp <- getCC
            libFlags <- getLibFlags
            incFlags <- getIncFlags
            let cc = comp ++ " " ++
                    ccStandard ++ " " ++
                    ccDbg dbg ++ " " ++
                    ccFlags ++
                    " -I. " ++ objs ++ " -x c++ " ++
                    (if (outputType == Executable) then "" else " -c ") ++
                    " " ++ tmpn ++
                    " " ++ libStandard ++ " " ++
                    " " ++ libFlags ++
                    " " ++ incFlags ++
                    " " ++ libs ++
                    " " ++ flags ++
                    " -o " ++ filename
            exit <- system cc
            when (exit /= ExitSuccess) $
              putStrLn ("FAILURE: " ++ cc)
    where

      headers xs = concatMap (\h -> let header = case h of ('<':_) -> h
                                                           _ -> "\"" ++ h ++ "\"" in                                                      
                                    "#include " ++ header ++ "\n")
                             (xs ++ []) -- "idris_rts.h", etc..

      debug TRACE = "#define IDRIS_TRACE\n\n"
      debug _ = ""

      -- We're using signed integers now. Make sure we get consistent semantics
      -- out of them from cc. See e.g. http://thiemonagel.de/2010/01/signed-integer-overflow/
      ccFlags = " -fwrapv -fno-strict-overflow"

      ccStandard = "-std=c++11"
      libStandard = "-lc++"

      ccDbg DEBUG = "-g"
      ccDbg TRACE = "-O2"
      ccDbg _ = "-O2"      

      varDecl :: Cpp -> T.Text
      varDecl (CppAlloc name (Just (CppFunction _ _))) = T.pack $ "void " ++ name ++ "(IndexType,IndexType);\n"
      varDecl (CppAlloc name _) = T.pack $ "extern Value " ++ name ++ ";\n"
      varDecl _ = ""      
      
      deadCodeElim :: [Cpp] -> [Cpp]
      deadCodeElim cpp = concatMap collectFunctions cpp
        where
          collectFunctions :: Cpp -> [Cpp]
          collectFunctions fun@(CppAlloc name _)
            | name == translateName (sMN 0 "runMain") = [fun]

          collectFunctions fun@(CppAlloc name (Just (CppFunction _ body))) =
            let invokations = sum $ map (
                    \x -> execState (countInvokations name x) 0
                  ) cpp
             in if invokations == 0
                   then []
                   else [fun]

          countInvokations :: String -> Cpp -> State Int ()
          countInvokations name (CppAlloc _ (Just (CppFunction _ body))) =
            countInvokations name body

          countInvokations name (CppSeq seq) =
            void $ traverse (countInvokations name) seq

          countInvokations name (CppAssign _ rhs) =
            countInvokations name rhs

          countInvokations name (CppCond conds) =
            void $ traverse (
                runKleisli $ arr id *** Kleisli (countInvokations name)
              ) conds

          countInvokations name (CppSwitch _ conds def) =
            void $ traverse (
              runKleisli $ arr id *** Kleisli (countInvokations name)
            ) conds >> traverse (countInvokations name) def

          countInvokations name (CppApp lhs rhs) =
            void $ countInvokations name lhs >> traverse (countInvokations name) rhs

          countInvokations name (CppNew _ args) =
            void $ traverse (countInvokations name) args

          countInvokations name (CppArray args) =
            void $ traverse (countInvokations name) args

          countInvokations name (CppIdent name')
            | name == name' = get >>= put . (+1)
            | otherwise     = return ()

          countInvokations _ _ = return ()

      processFunction :: Cpp -> [Cpp]
      processFunction =
        collectSplitFunctions . (\x -> evalRWS (splitFunction x) () 0)

      main :: T.Text
      main = 
        compileCpp $ CppAlloc "main" (Just $ CppFunction [] mainFun)

      mainFun :: Cpp
      mainFun =
        CppSeq [ CppAlloc "vm" (Just $ CppNew "make_shared<VirtualMachine>" [])
              , CppApp (CppIdent "schedule") [CppIdent "vm"]
              , CppApp (
                  CppIdent (translateName (sMN 0 "runMain"))
                ) [CppNum (CppInt 0),CppNum (CppInt 0)]
              , CppWhile (CppProj cppCALLSTACK "size() > 0") (
                  CppSeq [ CppAlloc "func" (Just cppPOP)
                        , CppAlloc "args" (Just cppPOPARGS)
                        , CppApp (CppIdent "func") [CppIdent "get<0>(args)", CppIdent "get<1>(args)"]
                        ]
                )
              ]

      invokeMain :: T.Text
      invokeMain = compileCpp CppNoop

optimizeConstructors :: [Cpp] -> ([Cpp], [Cpp])
optimizeConstructors cpp =
    let (cpp', cons) = runState (traverse optimizeConstructor' cpp) M.empty in
        (map (allocCon . snd) (M.toList cons), cpp')
  where
    allocCon :: (String, Cpp) -> Cpp
    allocCon (name, con) = CppAlloc name (Just con)

    newConstructor :: Int -> String
    newConstructor n = "_con_" ++ show n

    optimizeConstructor' :: Cpp -> State (M.Map Int (String, Cpp)) Cpp
    optimizeConstructor' cpp@(CppNew "_con_" [ CppNum (CppInt tag)
                                           , CppArray []
                                           , a
                                           , e
                                           ]) = do
      s <- get
      case M.lookup tag s of
           Just (i, c) -> return $ CppIdent i
           Nothing     -> do let n = newConstructor tag
                             put $ M.insert tag (n, cpp) s
                             return $ CppIdent n

    optimizeConstructor' (CppSeq seq) =
      CppSeq <$> traverse optimizeConstructor' seq

    optimizeConstructor' (CppSwitch reg cond def) = do
      cond' <- traverse (runKleisli $ arr id *** Kleisli optimizeConstructor') cond
      def'  <- traverse optimizeConstructor' def
      return $ CppSwitch reg cond' def'

    optimizeConstructor' (CppCond cond) =
      CppCond <$> traverse (runKleisli $ arr id *** Kleisli optimizeConstructor') cond

    optimizeConstructor' (CppAlloc fun (Just (CppFunction args body))) = do
      body' <- optimizeConstructor' body
      return $ CppAlloc fun (Just (CppFunction args body'))

    optimizeConstructor' (CppAssign lhs rhs) = do
      lhs' <- optimizeConstructor' lhs
      rhs' <- optimizeConstructor' rhs
      return $ CppAssign lhs' rhs'

    optimizeConstructor' cpp = return cpp

collectSplitFunctions :: (Cpp, [(Int,Cpp)]) -> [Cpp]
collectSplitFunctions (fun, splits) = map generateSplitFunction splits ++ [fun]
  where
    generateSplitFunction :: (Int,Cpp) -> Cpp
    generateSplitFunction (depth, CppAlloc name fun) =
      CppAlloc (name ++ "_" ++ show depth) fun

splitFunction :: Cpp -> RWS () [(Int,Cpp)] Int Cpp
splitFunction (CppAlloc name (Just (CppFunction args body@(CppSeq _)))) = do
  body' <- splitSequence body
  return $ CppAlloc name (Just (CppFunction args body'))
    where
      splitCondition :: Cpp -> RWS () [(Int,Cpp)] Int Cpp
      splitCondition cpp
        | CppCond branches <- cpp =
            CppCond <$> processBranches branches
        | CppSwitch cond branches def <- cpp =
            CppSwitch cond <$> (processBranches branches) <*> (traverse splitSequence def)
        | otherwise = return cpp
        where
          processBranches :: [(Cpp,Cpp)] -> RWS () [(Int,Cpp)] Int [(Cpp,Cpp)]
          processBranches =
            traverse (runKleisli (arr id *** Kleisli splitSequence))

      splitSequence :: Cpp -> RWS () [(Int, Cpp)] Int Cpp
      splitSequence cpp@(CppSeq seq) =
        let (pre,post) = break isCall seq in
            case post of
                 []                    -> CppSeq <$> traverse splitCondition seq
                 [cpp@(CppCond _)]       -> splitCondition cpp
                 [cpp@(CppSwitch {})] -> splitCondition cpp
                 [_]                   -> return cpp
                 (call:rest) -> do
                   depth <- get
                   put (depth + 1)
                   new <- splitFunction (newFun rest)
                   tell [(depth, new)]
                   return $ CppSeq (pre ++ (newCall depth : [call]))

      splitSequence cpp = return cpp

      isCall :: Cpp -> Bool
      isCall (CppApp (CppIdent "vmcall") _) = True
      isCall _                            = False

      newCall :: Int -> Cpp
      newCall depth =
        CppApp (CppIdent "vmcall") [ CppIdent $ name ++ "_" ++ show depth
                                 , CppArray [cppOLDBASE, cppMYOLDBASE]
                                 ]

      newFun :: [Cpp] -> Cpp
      newFun seq =
        CppAlloc name (Just $ CppFunction ["IndexType oldbase", "IndexType myoldbase"] (CppSeq seq))

splitFunction cpp = return cpp

translateDecl :: CompileInfo -> (Name, [BC]) -> [Cpp]
translateDecl info (name@(MN 0 fun), bc)
  | txt "APPLY" == fun || txt "EVAL" == fun =
         allocCaseFunctions (snd body)
      ++ [ CppAlloc (
               translateName name
           ) (Just $ CppFunction ["IndexType oldbase", "IndexType myoldbase"] (
               CppSeq $ map (translateBC info) (fst body) ++ [
                 CppCond [ ( (translateReg $ caseReg (snd body)) `cppInstanceOf` "Con" `cppAnd` 
                              (CppPtrProj (CppPtrProj (translateReg $ caseReg (snd body)) "Con") "function")
                          , CppApp (CppPtrProj (CppPtrProj (translateReg $ caseReg (snd body)) "Con") "function") [cppOLDBASE, cppMYOLDBASE]
                          )
                          , ( CppNoop
                            , CppSeq $ map (translateBC info) (defaultCase (snd body))
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

    allocCaseFunctions :: [BC] -> [Cpp]
    allocCaseFunctions ((CASE _ _ c _):_) = splitBranches c
    allocCaseFunctions _                  = []

    splitBranches :: [(Int, [BC])] -> [Cpp]
    splitBranches = map prepBranch

    prepBranch :: (Int, [BC]) -> Cpp
    prepBranch (tag, code) =
      CppAlloc (
        translateName name ++ "_" ++ show tag
      ) (Just $ CppFunction ["IndexType oldbase", "IndexType myoldbase"] (
          CppSeq $ map (translateBC info) code
        )
      )

translateDecl info (name, bc) =
  [ CppAlloc (
       translateName name
     ) (Just $ CppFunction ["IndexType oldbase", "IndexType myoldbase"] (
         CppSeq $ map (translateBC info)bc
       )
     )
  ]


translateReg :: Reg -> Cpp
translateReg reg
  | RVal <- reg = cppRET
  | Tmp  <- reg = CppRaw "//TMPREG"
  | L n  <- reg = cppLOC n
  | T n  <- reg = cppTOP n

translateConstant :: Const -> Cpp
translateConstant (I i)                    = CppNum (CppInt i)
translateConstant (Fl f)                   = CppNum (CppFloat f)
translateConstant (Ch c)                   = CppChar (translateChar c)
translateConstant (Str s)                  = CppString $ concatMap translateChar s
translateConstant (AType (ATInt ITNative)) = CppType CppIntTy
translateConstant StrType                  = CppType CppStringTy
translateConstant (AType (ATInt ITBig))    = CppType CppIntegerTy
translateConstant (AType ATFloat)          = CppType CppFloatTy
translateConstant (AType (ATInt ITChar))   = CppType CppCharTy
translateConstant PtrType                  = CppType CppPtrTy
translateConstant Forgot                   = CppType CppForgotTy
translateConstant (BI 0)                   = CppNum $ CppInteger (CppBigInt 0)
translateConstant (BI 1)                   = CppNum $ CppInteger (CppBigInt 1)
translateConstant (BI i)                   = CppNum $ CppInteger (CppBigInt i)
translateConstant (B8 b)                   = CppWord (CppWord8 b)
translateConstant (B16 b)                  = CppWord (CppWord16 b)
translateConstant (B32 b)                  = CppWord (CppWord32 b)
translateConstant (B64 b)                  = CppWord (CppWord64 b)
translateConstant c =
  CppError $ "Unimplemented Constant: " ++ show c


translateChar :: Char -> String
translateChar ch = "\\x" ++ fill (showHex (ord ch) "")
  -- | '\a'   <- ch       = "\\a"
  -- | '\b'   <- ch       = "\\b"
  -- | '\f'   <- ch       = "\\f"
  -- | '\n'   <- ch       = "\\n"
  -- | '\r'   <- ch       = "\\r"
  -- | '\t'   <- ch       = "\\t"
  -- | '\v'   <- ch       = "\\v"
  -- | '\SO'  <- ch       = "\\u000E"
  -- | '\DEL' <- ch       = "\\u007F"
  -- | '\\'   <- ch       = "\\u005C"
  -- | '\"'   <- ch       = "\\u0022"
  -- | '\''   <- ch       = "\\u002C"
  -- | ch `elem` asciiTab = "\\u00" ++ fill (showHex (ord ch) "")
  -- | otherwise          = [ch]
  where
    fill :: String -> String
    fill s = if length s == 1
                then '0' : s
                else s
  --
  --   asciiTab =
  --     ['\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK', '\BEL',
  --      '\BS',  '\HT',  '\LF',  '\VT',  '\FF',  '\CR',  '\SO',  '\SI',
  --      '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB',
  --      '\CAN', '\EM',  '\SUB', '\ESC', '\FS',  '\GS',  '\RS',  '\US']

translateName :: Name -> String
translateName n = "_idris_" ++ concatMap cchar (showCG n)
  where cchar x | isAlphaNum x = [x]
                | otherwise    = "_" ++ show (fromEnum x) ++ "_"

cppASSIGN :: CompileInfo -> Reg -> Reg -> Cpp
cppASSIGN _ r1 r2 = CppAssign (translateReg r1) (translateReg r2)

cppASSIGNCONST :: CompileInfo -> Reg -> Const -> Cpp
cppASSIGNCONST _ r c = CppAssign (translateReg r) (cppBOX $ translateConstant c)

cppCALL :: CompileInfo -> Name -> Cpp
cppCALL _ n =
  CppApp (
    CppIdent "vmcall"
  ) [CppIdent (translateName n), CppArray [cppMYOLDBASE,CppNum (CppInt 0)]]

cppTAILCALL :: CompileInfo -> Name -> Cpp
cppTAILCALL _ n =
  CppApp (
    CppIdent "vmcall"
  ) [CppIdent (translateName n), CppArray [cppOLDBASE,CppNum (CppInt 0)]]

cppFOREIGN :: CompileInfo -> Reg -> String -> [(FType, Reg)] -> Cpp
cppFOREIGN _ reg n args
  | n == "putStr"
  , [(FString, arg)] <- args =
      CppAssign (
        translateReg reg
      ) (
        CppParens $ CppBinOp "," (CppBinOp "<<" (CppIdent "cout") 
                                                   (cppUNBOXED (translateReg arg) "string" ))
                                   CppNull
      )

  | n == "isNull"
  , [(FPtr, arg)] <- args =
      CppAssign (
        translateReg reg
      ) (
        CppBinOp "==" (translateReg arg) CppNull
      )

  | n == "idris_eqPtr"
  , [(_, lhs),(_, rhs)] <- args =
      CppAssign (
        translateReg reg
      ) (
        CppBinOp "==" (translateReg lhs) (translateReg rhs)
      )
  | otherwise =
     CppAssign (
       translateReg reg
     ) (
       cppBOX $ CppFFI n (map generateWrapper args)
     )
    where
      generateWrapper :: (FType, Reg) -> Cpp
      generateWrapper (ty, reg)
        | FFunction aty rty <- ty =
            CppApp (CppIdent $ "LAMBDA_WRAPPER") [translateReg reg, cType aty, cType rty]

        -- | FFunctionIO <- ty =
        --     CppApp (CppIdent "i_ffiWrap") [ translateReg reg
        --                                 , CppIdent "oldbase"
        --                                 , CppIdent "myoldbase"
        --                                 ]
      generateWrapper (ty, reg) =
        cppUNBOXED (translateReg reg) (T.unpack . compileCpp $ cType ty)
        
      cType :: FType -> Cpp
      cType (FArith (ATInt ITNative))       = CppIdent "int"
      cType (FArith (ATInt ITChar))         = CppIdent "char"
      cType (FArith (ATInt (ITFixed ity)))  = CppIdent "int"
      cType (FArith (ATInt ITBig))          = CppIdent "long long"
      cType (FArith (ATInt (ITFixed IT8)))  = CppIdent "int8_t"
      cType (FArith (ATInt (ITFixed IT16))) = CppIdent "int16_t"
      cType (FArith (ATInt (ITFixed IT32))) = CppIdent "int32_t"
      cType (FArith (ATInt (ITFixed IT64))) = CppIdent "int64_t"
      cType FString = CppIdent "string"
      cType FUnit = CppIdent "void"
      cType FPtr = CppIdent "void*"
      cType FManagedPtr = CppIdent "void *"
      cType (FArith ATFloat) = CppIdent "double"
      cType FAny = CppIdent "void*"
      cType (FFunction a b) = CppList [cType a, cType b]

cppREBASE :: CompileInfo -> Cpp
cppREBASE _ = CppAssign cppSTACKBASE cppOLDBASE

cppSTOREOLD :: CompileInfo ->Cpp
cppSTOREOLD _ = CppAssign cppMYOLDBASE cppSTACKBASE

cppADDTOP :: CompileInfo -> Int -> Cpp
cppADDTOP info n
  | 0 <- n    = CppNoop
  | otherwise =
      CppBinOp "+=" cppSTACKTOP (CppNum (CppInt n))

cppTOPBASE :: CompileInfo -> Int -> Cpp
cppTOPBASE _ 0  = CppAssign cppSTACKTOP cppSTACKBASE
cppTOPBASE _ n  = CppAssign cppSTACKTOP (CppBinOp "+" cppSTACKBASE (CppNum (CppInt n)))


cppBASETOP :: CompileInfo -> Int -> Cpp
cppBASETOP _ 0 = CppAssign cppSTACKBASE cppSTACKTOP
cppBASETOP _ n = CppAssign cppSTACKBASE (CppBinOp "+" cppSTACKTOP (CppNum (CppInt n)))

cppNULL :: CompileInfo -> Reg -> Cpp
cppNULL _ r = CppAssign (translateReg r) CppNull

cppERROR :: CompileInfo -> String -> Cpp
cppERROR _ = CppError

cppSLIDE :: CompileInfo -> Int -> Cpp
cppSLIDE _ 1 = CppAssign (cppLOC 0) (cppTOP 0)
cppSLIDE _ n = CppApp (CppIdent "slide") [CppNum (CppInt n)]

cppMKCON :: CompileInfo -> Reg -> Int -> [Reg] -> Cpp
cppMKCON info r t rs =
  CppAssign (translateReg r) (
    CppApp (CppIdent "MakeCon") 
            [ CppNum (CppInt t)
                  , CppArray (map translateReg rs)
                  , if t `elem` compileInfoApplyCases info
                       then CppIdent $ translateName (sMN 0 "APPLY") ++ "_" ++ show t
                    else if t `elem` compileInfoEvalCases info
                       then CppIdent $ translateName (sMN 0 "EVAL") ++ "_" ++ show t
                    else CppNull
                  ]
  )

cppCASE :: CompileInfo -> Bool -> Reg -> [(Int, [BC])] -> Maybe [BC] -> Cpp
cppCASE info safe reg cases def =
  CppSwitch (tag safe $ translateReg reg) (
    map ((CppNum . CppInt) *** prepBranch) cases
  ) (fmap prepBranch def)
    where
      tag :: Bool -> Cpp -> Cpp
      tag True  = cppCTAG
      tag False = cppTAG

      prepBranch :: [BC] -> Cpp
      prepBranch bc = CppSeq $ map (translateBC info) bc

      cppTAG :: Cpp -> Cpp
      cppTAG cpp =
        (CppTernary (cpp `cppInstanceOf` "Con") (
          CppPtrProj cpp "tag"
        ) (CppNum (CppInt $ negate 1)))

      cppCTAG :: Cpp -> Cpp
      cppCTAG cpp = CppPtrProj (CppPtrProj cpp "Con") "tag"


cppCONSTCASE :: CompileInfo -> Reg -> [(Const, [BC])] -> Maybe [BC] -> Cpp
cppCONSTCASE info reg cases def =
  CppCond $ (
    map (unboxedBinOp (cppEq) (translateReg reg) . translateConstant *** prepBranch) cases
  ) ++ (maybe [] ((:[]) . ((,) CppNoop) . prepBranch) def)
    where
      prepBranch :: [BC] -> Cpp
      prepBranch bc = CppSeq $ map (translateBC info) bc

      unboxedBinOp :: (Cpp -> Cpp -> Cpp) -> Cpp -> Cpp -> Cpp
      unboxedBinOp f l r = f (cppUNBOXED l (unboxedType r)) r
      
cppPROJECT :: CompileInfo -> Reg -> Int -> Int -> Cpp
cppPROJECT _ reg loc 0  = CppNoop
cppPROJECT _ reg loc 1  =
  CppAssign (cppLOC loc) (
    CppIndex (CppPtrProj (CppPtrProj (translateReg reg) "Con") "args")
              (CppNum $ CppInt 0)
  )
cppPROJECT _ reg loc ar =
  CppApp (CppIdent "project") [ translateReg reg
                              , CppNum (CppInt loc)
                              , CppNum (CppInt ar)
                              ]

cppOP :: CompileInfo -> Reg -> PrimFn -> [Reg] -> Cpp
cppOP _ reg op args = CppAssign (translateReg reg) cppOP'
  where
    cppOP' :: Cpp
    cppOP'
      | LNoOp <- op = translateReg (last args)

      | (LZExt (ITFixed IT8) ITNative)  <- op = cppUnPackUBits8 $ translateReg (last args)
      | (LZExt (ITFixed IT16) ITNative) <- op = cppUnPackUBits16 $ translateReg (last args)
      | (LZExt (ITFixed IT32) ITNative) <- op = cppUnPackUBits32 $ translateReg (last args)
      | (LZExt (ITFixed IT64) ITNative) <- op = cppUnPackUBits64 $ translateReg (last args)

      | (LZExt _ ITBig)        <- op = translateReg (last args)
      | (LPlus (ATInt ITBig))  <- op
      , (lhs:rhs:_)            <- args = translateBinaryOp "+" lhs rhs
      | (LMinus (ATInt ITBig)) <- op
      , (lhs:rhs:_)            <- args = translateBinaryOp "-" lhs rhs
      | (LTimes (ATInt ITBig)) <- op
      , (lhs:rhs:_)            <- args = translateBinaryOp "*" lhs rhs
      | (LSDiv (ATInt ITBig))  <- op
      , (lhs:rhs:_)            <- args = translateBinaryOp "/" lhs rhs
      | (LSRem (ATInt ITBig))  <- op
      , (lhs:rhs:_)            <- args = translateBinaryOp "%" lhs rhs
      | (LEq (ATInt ITBig))    <- op
      , (lhs:rhs:_)            <- args = translateBinaryOp "==" lhs rhs
      | (LSLt (ATInt ITBig))   <- op
      , (lhs:rhs:_)            <- args = translateBinaryOp "<" lhs rhs
      | (LSLe (ATInt ITBig))   <- op
      , (lhs:rhs:_)            <- args = translateBinaryOp "<=" lhs rhs
      | (LSGt (ATInt ITBig))   <- op
      , (lhs:rhs:_)            <- args = translateBinaryOp ">" lhs rhs
      | (LSGe (ATInt ITBig))   <- op
      , (lhs:rhs:_)            <- args = translateBinaryOp ">=" lhs rhs

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
          cppCall "fromCharCode" [
            CppBinOp "+" (
              cppCall "charCode" [translateReg lhs]
            ) (
              cppCall "charCode" [translateReg rhs]
            )
          ]

      | (LTrunc (ITFixed IT16) (ITFixed IT8)) <- op
      , (arg:_)                               <- args =
          cppPackUBits8 (
            CppBinOp "&" (cppUnPackUBits8 $ translateReg arg) (CppNum (CppInt 0xFF))
          )

      | (LTrunc (ITFixed IT32) (ITFixed IT16)) <- op
      , (arg:_)                                <- args =
          cppPackUBits16 (
            CppBinOp "&" (cppUnPackUBits16 $ translateReg arg) (CppNum (CppInt 0xFFFF))
          )

      | (LTrunc (ITFixed IT64) (ITFixed IT32)) <- op
      , (arg:_)                                <- args =
          cppPackUBits32 (
            CppBinOp "&" (cppUnPackUBits32 $ translateReg arg) (CppNum (CppInt 0xFFFFFFFF))
          )

      | (LTrunc ITBig (ITFixed IT64)) <- op
      , (arg:_)                       <- args =
          cppPackUBits64 (
            CppBinOp "&" (cppUnPackUBits64 $ translateReg arg) (CppNum $ CppInteger (CppBigInt 0xFFFFFFFFFFFFFFFF))
          )

      | (LLSHR (ITFixed IT8)) <- op
      , (lhs:rhs:_)           <- args =
          cppPackUBits8 (
            CppBinOp ">>" (cppUnPackUBits8 $ translateReg lhs) (cppUnPackUBits8 $ translateReg rhs)
          )

      | (LLSHR (ITFixed IT16)) <- op
      , (lhs:rhs:_)            <- args =
          cppPackUBits16 (
            CppBinOp ">>" (cppUnPackUBits16 $ translateReg lhs) (cppUnPackUBits16 $ translateReg rhs)
          )

      | (LLSHR (ITFixed IT32)) <- op
      , (lhs:rhs:_)            <- args =
          cppPackUBits32  (
            CppBinOp ">>" (cppUnPackUBits32 $ translateReg lhs) (cppUnPackUBits32 $ translateReg rhs)
          )

      | (LLSHR (ITFixed IT64)) <- op
      , (lhs:rhs:_)            <- args =
          cppPackUBits64  (
            CppBinOp ">>" (cppUnPackUBits64 $ translateReg lhs) (cppUnPackUBits64 $ translateReg rhs)
          )

      | (LSHL (ITFixed IT8)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits8 (
            CppBinOp "<<" (cppUnPackUBits8 $ translateReg lhs) (cppUnPackUBits8 $ translateReg rhs)
          )

      | (LSHL (ITFixed IT16)) <- op
      , (lhs:rhs:_)           <- args =
          cppPackUBits16 (
            CppBinOp "<<" (cppUnPackUBits16 $ translateReg lhs) (cppUnPackUBits16 $ translateReg rhs)
          )

      | (LSHL (ITFixed IT32)) <- op
      , (lhs:rhs:_)           <- args =
          cppPackUBits32  (
            CppBinOp "<<" (cppUnPackUBits32 $ translateReg lhs) (cppUnPackUBits32 $ translateReg rhs)
          )

      | (LSHL (ITFixed IT64)) <- op
      , (lhs:rhs:_)           <- args =
          cppPackUBits64  (
            CppBinOp "<<" (cppUnPackUBits64 $ translateReg lhs) (cppUnPackUBits64 $ translateReg rhs)
          )

      | (LAnd (ITFixed IT8)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits8 (
            CppBinOp "&" (cppUnPackUBits8 (translateReg lhs)) (cppUnPackUBits8 (translateReg rhs))
          )

      | (LAnd (ITFixed IT16)) <- op
      , (lhs:rhs:_)           <- args =
          cppPackUBits16 (
            CppBinOp "&" (cppUnPackUBits16 (translateReg lhs)) (cppUnPackUBits16 (translateReg rhs))
          )

      | (LAnd (ITFixed IT32)) <- op
      , (lhs:rhs:_)           <- args =
          cppPackUBits32 (
            CppBinOp "&" (cppUnPackUBits32 (translateReg lhs)) (cppUnPackUBits32 (translateReg rhs))
          )

      | (LAnd (ITFixed IT64)) <- op
      , (lhs:rhs:_)           <- args =
          cppPackUBits64 (
            CppBinOp "&" (cppUnPackUBits64 (translateReg lhs)) (cppUnPackUBits64 (translateReg rhs))
          )

      | (LOr (ITFixed IT8)) <- op
      , (lhs:rhs:_)         <- args =
          cppPackUBits8 (
            CppBinOp "|" (cppUnPackUBits8 (translateReg lhs)) (cppUnPackUBits8 (translateReg rhs))
          )

      | (LOr (ITFixed IT16)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits16 (
            CppBinOp "|" (cppUnPackUBits16 (translateReg lhs)) (cppUnPackUBits16 (translateReg rhs))
          )

      | (LOr (ITFixed IT32)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits32 (
            CppBinOp "|" (cppUnPackUBits32 (translateReg lhs)) (cppUnPackUBits32 (translateReg rhs))
          )

      | (LOr (ITFixed IT64)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits64 (
            CppBinOp "|" (cppUnPackUBits64 (translateReg lhs)) (cppUnPackUBits64 (translateReg rhs))
          )

      | (LXOr (ITFixed IT8)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits8 (
            CppBinOp "^" (cppUnPackUBits8 (translateReg lhs)) (cppUnPackUBits8 (translateReg rhs))
          )

      | (LXOr (ITFixed IT16)) <- op
      , (lhs:rhs:_)           <- args =
          cppPackUBits16 (
            CppBinOp "^" (cppUnPackUBits16 (translateReg lhs)) (cppUnPackUBits16 (translateReg rhs))
          )

      | (LXOr (ITFixed IT32)) <- op
      , (lhs:rhs:_)           <- args =
          cppPackUBits32 (
            CppBinOp "^" (cppUnPackUBits32 (translateReg lhs)) (cppUnPackUBits32 (translateReg rhs))
          )

      | (LXOr (ITFixed IT64)) <- op
      , (lhs:rhs:_)           <- args =
          cppPackUBits64 (
            CppBinOp "^" (cppUnPackUBits64 (translateReg lhs)) (cppUnPackUBits64 (translateReg rhs))
          )

      | (LPlus (ATInt (ITFixed IT8))) <- op
      , (lhs:rhs:_)                   <- args =
          cppPackUBits8 (
            CppBinOp "+" (cppUnPackUBits8 (translateReg lhs)) (cppUnPackUBits8 (translateReg rhs))
          )

      | (LPlus (ATInt (ITFixed IT16))) <- op
      , (lhs:rhs:_)                    <- args =
          cppPackUBits16 (
            CppBinOp "+" (cppUnPackUBits16 (translateReg lhs)) (cppUnPackUBits16 (translateReg rhs))
          )

      | (LPlus (ATInt (ITFixed IT32))) <- op
      , (lhs:rhs:_)                    <- args =
          cppPackUBits32 (
            CppBinOp "+" (cppUnPackUBits32 (translateReg lhs)) (cppUnPackUBits32 (translateReg rhs))
          )

      | (LPlus (ATInt (ITFixed IT64))) <- op
      , (lhs:rhs:_)                    <- args =
          cppPackUBits64 (
            CppBinOp "+" (cppUnPackUBits64 (translateReg lhs)) (cppUnPackUBits64 (translateReg rhs))
          )

      | (LMinus (ATInt (ITFixed IT8))) <- op
      , (lhs:rhs:_)                    <- args =
          cppPackUBits8 (
            CppBinOp "-" (cppUnPackUBits8 (translateReg lhs)) (cppUnPackUBits8 (translateReg rhs))
          )

      | (LMinus (ATInt (ITFixed IT16))) <- op
      , (lhs:rhs:_)                     <- args =
          cppPackUBits16 (
            CppBinOp "-" (cppUnPackUBits16 (translateReg lhs)) (cppUnPackUBits16 (translateReg rhs))
          )

      | (LMinus (ATInt (ITFixed IT32))) <- op
      , (lhs:rhs:_)                     <- args =
          cppPackUBits32 (
            CppBinOp "-" (cppUnPackUBits32 (translateReg lhs)) (cppUnPackUBits32 (translateReg rhs))
          )

      | (LMinus (ATInt (ITFixed IT64))) <- op
      , (lhs:rhs:_)                     <- args =
          cppPackUBits64 (
            CppBinOp "-" (cppUnPackUBits64 (translateReg lhs)) (cppUnPackUBits64 (translateReg rhs))
          )

      | (LTimes (ATInt (ITFixed IT8))) <- op
      , (lhs:rhs:_)                    <- args =
          cppPackUBits8 (
            CppBinOp "*" (cppUnPackUBits8 (translateReg lhs)) (cppUnPackUBits8 (translateReg rhs))
          )

      | (LTimes (ATInt (ITFixed IT16))) <- op
      , (lhs:rhs:_)                     <- args =
          cppPackUBits16 (
            CppBinOp "*" (cppUnPackUBits16 (translateReg lhs)) (cppUnPackUBits16 (translateReg rhs))
          )

      | (LTimes (ATInt (ITFixed IT32))) <- op
      , (lhs:rhs:_)                     <- args =
          cppPackUBits32 (
            CppBinOp "*" (cppUnPackUBits32 (translateReg lhs)) (cppUnPackUBits32 (translateReg rhs))
          )

      | (LTimes (ATInt (ITFixed IT64))) <- op
      , (lhs:rhs:_)                     <- args =
          cppPackUBits64 (
            CppBinOp "*" (cppUnPackUBits64 (translateReg lhs)) (cppUnPackUBits64 (translateReg rhs))
          )

      | (LEq (ATInt (ITFixed IT8))) <- op
      , (lhs:rhs:_)                 <- args =
          cppPackUBits8 (
            CppBinOp "==" (cppUnPackUBits8 (translateReg lhs)) (cppUnPackUBits8 (translateReg rhs))
          )

      | (LEq (ATInt (ITFixed IT16))) <- op
      , (lhs:rhs:_)                  <- args =
          cppPackUBits16 (
            CppBinOp "==" (cppUnPackUBits16 (translateReg lhs)) (cppUnPackUBits16 (translateReg rhs))
          )

      | (LEq (ATInt (ITFixed IT32))) <- op
      , (lhs:rhs:_)                  <- args =
          cppPackUBits32 (
            CppBinOp "==" (cppUnPackUBits32 (translateReg lhs)) (cppUnPackUBits32 (translateReg rhs))
          )

      | (LEq (ATInt (ITFixed IT64))) <- op
      , (lhs:rhs:_)                   <- args =
          cppPackUBits64 (
            CppBinOp "==" (cppUnPackUBits64 (translateReg lhs)) (cppUnPackUBits64 (translateReg rhs))
          )

      | (LLt (ITFixed IT8)) <- op
      , (lhs:rhs:_)         <- args =
          cppPackUBits8 (
            CppBinOp "<" (cppUnPackUBits8 (translateReg lhs)) (cppUnPackUBits8 (translateReg rhs))
          )

      | (LLt (ITFixed IT16)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits16 (
            CppBinOp "<" (cppUnPackUBits16 (translateReg lhs)) (cppUnPackUBits16 (translateReg rhs))
          )

      | (LLt (ITFixed IT32)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits32 (
            CppBinOp "<" (cppUnPackUBits32 (translateReg lhs)) (cppUnPackUBits32 (translateReg rhs))
          )

      | (LLt (ITFixed IT64)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits64 (
            CppBinOp "<" (cppUnPackUBits64 (translateReg lhs)) (cppUnPackUBits64 (translateReg rhs))
          )

      | (LLe (ITFixed IT8)) <- op
      , (lhs:rhs:_)         <- args =
          cppPackUBits8 (
            CppBinOp "<=" (cppUnPackUBits8 (translateReg lhs)) (cppUnPackUBits8 (translateReg rhs))
          )

      | (LLe (ITFixed IT16)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits16 (
            CppBinOp "<=" (cppUnPackUBits16 (translateReg lhs)) (cppUnPackUBits16 (translateReg rhs))
          )

      | (LLe (ITFixed IT32)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits32 (
            CppBinOp "<=" (cppUnPackUBits32 (translateReg lhs)) (cppUnPackUBits32 (translateReg rhs))
          )

      | (LLe (ITFixed IT64)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits64 (
            CppBinOp "<=" (cppUnPackUBits64 (translateReg lhs)) (cppUnPackUBits64 (translateReg rhs))
          )

      | (LGt (ITFixed IT8)) <- op
      , (lhs:rhs:_)         <- args =
          cppPackUBits8 (
            CppBinOp ">" (cppUnPackUBits8 (translateReg lhs)) (cppUnPackUBits8 (translateReg rhs))
          )

      | (LGt (ITFixed IT16)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits16 (
            CppBinOp ">" (cppUnPackUBits16 (translateReg lhs)) (cppUnPackUBits16 (translateReg rhs))
          )
      | (LGt (ITFixed IT32)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits32 (
            CppBinOp ">" (cppUnPackUBits32 (translateReg lhs)) (cppUnPackUBits32 (translateReg rhs))
          )

      | (LGt (ITFixed IT64)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits64 (
            CppBinOp ">" (cppUnPackUBits64 (translateReg lhs)) (cppUnPackUBits64 (translateReg rhs))
          )

      | (LGe (ITFixed IT8)) <- op
      , (lhs:rhs:_)         <- args =
          cppPackUBits8 (
            CppBinOp ">=" (cppUnPackUBits8 (translateReg lhs)) (cppUnPackUBits8 (translateReg rhs))
          )

      | (LGe (ITFixed IT16)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits16 (
            CppBinOp ">=" (cppUnPackUBits16 (translateReg lhs)) (cppUnPackUBits16 (translateReg rhs))
          )
      | (LGe (ITFixed IT32)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits32 (
            CppBinOp ">=" (cppUnPackUBits32 (translateReg lhs)) (cppUnPackUBits32 (translateReg rhs))
          )

      | (LGe (ITFixed IT64)) <- op
      , (lhs:rhs:_)          <- args =
          cppPackUBits64 (
            CppBinOp ">=" (cppUnPackUBits64 (translateReg lhs)) (cppUnPackUBits64 (translateReg rhs))
          )

      | (LUDiv (ITFixed IT8)) <- op
      , (lhs:rhs:_)           <- args =
          cppPackUBits8 (
            CppBinOp "/" (cppUnPackUBits8 (translateReg lhs)) (cppUnPackUBits8 (translateReg rhs))
          )

      | (LUDiv (ITFixed IT16)) <- op
      , (lhs:rhs:_)            <- args =
          cppPackUBits16 (
            CppBinOp "/" (cppUnPackUBits16 (translateReg lhs)) (cppUnPackUBits16 (translateReg rhs))
          )

      | (LUDiv (ITFixed IT32)) <- op
      , (lhs:rhs:_)            <- args =
          cppPackUBits32 (
            CppBinOp "/" (cppUnPackUBits32 (translateReg lhs)) (cppUnPackUBits32 (translateReg rhs))
          )

      | (LUDiv (ITFixed IT64)) <- op
      , (lhs:rhs:_)            <- args =
          cppPackUBits64 (
            CppBinOp "/" (cppUnPackUBits64 (translateReg lhs)) (cppUnPackUBits64 (translateReg rhs))
          )

      | (LSDiv (ATInt (ITFixed IT8))) <- op
      , (lhs:rhs:_)                   <- args =
          cppPackSBits8 (
            CppBinOp "/" (
              cppUnPackSBits8 $ cppPackSBits8 $ cppUnPackSBits8 (translateReg lhs)
            ) (
              cppUnPackSBits8 $ cppPackSBits8 $ cppUnPackSBits8 (translateReg rhs)
            )
          )

      | (LSDiv (ATInt (ITFixed IT16))) <- op
      , (lhs:rhs:_)                    <- args =
          cppPackSBits16 (
            CppBinOp "/" (
              cppUnPackSBits16 $ cppPackSBits16 $ cppUnPackSBits16 (translateReg lhs)
            ) (
              cppUnPackSBits16 $ cppPackSBits16 $ cppUnPackSBits16 (translateReg rhs)
            )
          )

      | (LSDiv (ATInt (ITFixed IT32))) <- op
      , (lhs:rhs:_)                    <- args =
          cppPackSBits32 (
            CppBinOp "/" (
              cppUnPackSBits32 $ cppPackSBits32 $ cppUnPackSBits32 (translateReg lhs)
            ) (
              cppUnPackSBits32 $ cppPackSBits32 $ cppUnPackSBits32 (translateReg rhs)
            )
          )

      | (LSDiv (ATInt (ITFixed IT64))) <- op
      , (lhs:rhs:_)                    <- args =
          cppPackSBits64 (
            CppBinOp "/" (
              cppUnPackSBits64 $ cppPackSBits64 $ cppUnPackSBits64 (translateReg lhs)
            ) (
              cppUnPackSBits64 $ cppPackSBits64 $ cppUnPackSBits64 (translateReg rhs)
            )
          )

      | (LSRem (ATInt (ITFixed IT8))) <- op
      , (lhs:rhs:_)                   <- args =
          cppPackSBits8 (
            CppBinOp "%" (
              cppUnPackSBits8 $ cppPackSBits8 $ cppUnPackSBits8 (translateReg lhs)
            ) (
              cppUnPackSBits8 $ cppPackSBits8 $ cppUnPackSBits8 (translateReg rhs)
            )
          )

      | (LSRem (ATInt (ITFixed IT16))) <- op
      , (lhs:rhs:_)                    <- args =
          cppPackSBits16 (
            CppBinOp "%" (
              cppUnPackSBits16 $ cppPackSBits16 $ cppUnPackSBits16 (translateReg lhs)
            ) (
              cppUnPackSBits16 $ cppPackSBits16 $ cppUnPackSBits16 (translateReg rhs)
            )
          )

      | (LSRem (ATInt (ITFixed IT32))) <- op
      , (lhs:rhs:_)                    <- args =
          cppPackSBits32 (
            CppBinOp "%" (
              cppUnPackSBits32 $ cppPackSBits32 $ cppUnPackSBits32 (translateReg lhs)
            ) (
              cppUnPackSBits32 $ cppPackSBits32 $ cppUnPackSBits32 (translateReg rhs)
            )
          )

      | (LSRem (ATInt (ITFixed IT64))) <- op
      , (lhs:rhs:_)                    <- args =
          cppPackSBits64 (
            CppBinOp "%" (
              cppUnPackSBits64 $ cppPackSBits64 $ cppUnPackSBits64 (translateReg lhs)
            ) (
              cppUnPackSBits64 $ cppPackSBits64 $ cppUnPackSBits64 (translateReg rhs)
            )
          )

      | (LCompl (ITFixed IT8)) <- op
      , (arg:_)                <- args =
          cppPackSBits8 $ CppPreOp "~" $ cppUnPackSBits8 (translateReg arg)

      | (LCompl (ITFixed IT16)) <- op
      , (arg:_)                 <- args =
          cppPackSBits16 $ CppPreOp "~" $ cppUnPackSBits16 (translateReg arg)

      | (LCompl (ITFixed IT32)) <- op
      , (arg:_)                 <- args =
          cppPackSBits32 $ CppPreOp "~" $ cppUnPackSBits32 (translateReg arg)

      | (LCompl (ITFixed IT64)) <- op
      , (arg:_)     <- args =
          cppPackSBits64 $ CppPreOp "~" $ cppUnPackSBits64 (translateReg arg)

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
      , (arg:_)     <- args = CppPreOp "~" (translateReg arg)

      | LStrConcat  <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "+" lhs rhs
      | LStrEq      <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "==" lhs rhs
      | LStrLt      <- op
      , (lhs:rhs:_) <- args = translateBinaryOp "<" lhs rhs
      | LStrLen     <- op
      , (arg:_)     <- args = cppBOXTYPE (strLen (cppUNBOXED (translateReg arg) "string")) "long long int"
      | (LStrInt ITNative)      <- op
      , (arg:_)                 <- args = cppCall "stoi" [cppUNBOXED (translateReg arg) "string"]
      | (LIntStr ITNative)      <- op
      , (arg:_)                 <- args = cppCall "to_string" [translateReg arg]
      | (LSExt ITNative ITBig)  <- op
      , (arg:_)                 <- args = translateReg arg
      | (LTrunc ITBig ITNative) <- op
      , (arg:_)                 <- args = cppCall "stoi" [cppUNBOXED (translateReg arg) "string"]
      | (LIntStr ITBig)         <- op
      , (arg:_)                 <- args = cppCall "to_string" [translateReg arg]
      | (LStrInt ITBig)         <- op
      , (arg:_)                 <- args = cppCall "stoll" [cppUNBOXED (translateReg arg) "string"]
      | LFloatStr               <- op
      , (arg:_)                 <- args = cppCall "to_string" [translateReg arg]
      | LStrFloat               <- op
      , (arg:_)                 <- args = cppCall "stod" [cppUNBOXED (translateReg arg) "string"]
      | (LIntFloat ITNative)    <- op
      , (arg:_)                 <- args = translateReg arg
      | (LFloatInt ITNative)    <- op
      , (arg:_)                 <- args = translateReg arg
      | (LChInt ITNative)       <- op
      , (arg:_)                 <- args = cppCall "charCode" [translateReg arg]
      | (LIntCh ITNative)       <- op
      , (arg:_)                 <- args = cppCall "fromCharCode" [translateReg arg]

      | LFExp       <- op
      , (arg:_)     <- args = cppCall "exp" [translateReg arg]
      | LFLog       <- op
      , (arg:_)     <- args = cppCall "log" [translateReg arg]
      | LFSin       <- op
      , (arg:_)     <- args = cppCall "sin" [translateReg arg]
      | LFCos       <- op
      , (arg:_)     <- args = cppCall "cos" [translateReg arg]
      | LFTan       <- op
      , (arg:_)     <- args = cppCall "tan" [translateReg arg]
      | LFASin      <- op
      , (arg:_)     <- args = cppCall "asin" [translateReg arg]
      | LFACos      <- op
      , (arg:_)     <- args = cppCall "acos" [translateReg arg]
      | LFATan      <- op
      , (arg:_)     <- args = cppCall "atan" [translateReg arg]
      | LFSqrt      <- op
      , (arg:_)     <- args = cppCall "sqrt" [translateReg arg]
      | LFFloor     <- op
      , (arg:_)     <- args = cppMeth (translateReg arg) "floor" []
      | LFCeil      <- op
      , (arg:_)     <- args = cppMeth (translateReg arg) "ceil" []

      | LStrCons    <- op
      , (lhs:rhs:_) <- args = cppBOX $ CppBinOp "+" (cppUNBOXED (translateReg lhs) "string") 
                                                    (cppUNBOXED (translateReg rhs) "string")
      | LStrHead    <- op
      , (arg:_)     <- args = 
          let str = cppUNBOXED (translateReg arg) "string" in      
              CppTernary (cppAnd (translateReg arg) (CppPreOp "!" (cppMeth str "empty" [])))
                         (cppBOXTYPE (cppMeth str "front" []) "character")
                         (CppNull)

      | LStrRev     <- op
      , (arg:_)     <- args = cppCall "reverse" [cppUNBOXED (translateReg arg) "string"]
      | LStrIndex   <- op
      , (lhs:rhs:_) <- args = cppBOX $ CppIndex (cppUNBOXED (translateReg lhs) "string") 
                                                (cppUNBOXED (translateReg rhs) "int")
      | LStrTail    <- op
      , (arg:_)     <- args =
          let v = cppUNBOXED (translateReg arg) "string" in
              cppBOX $ CppTernary (cppAnd (translateReg arg) (cppGreaterThan (strLen v) cppOne))
                                  (cppMeth v "substr" [cppOne, CppBinOp "-" (strLen v) cppOne])
                                  (CppString "")

      | LSystemInfo <- op
      , (arg:_) <- args = cppBOX $ cppCall "systemInfo"  [translateReg arg]
      | LNullPtr    <- op
      , (_)         <- args = CppNull
      | otherwise = CppError $ "Not implemented: " ++ show op
        where
          translateBinaryOp :: String -> Reg -> Reg -> Cpp
          translateBinaryOp op lhs rhs =
            CppBinOp op (translateReg lhs) (translateReg rhs)

          invokeMeth :: Reg -> String -> [Reg] -> Cpp
          invokeMeth obj meth args =
            CppApp (CppProj (translateReg obj) meth) $ map translateReg args
            
          strLen :: Cpp -> Cpp
          strLen s = cppMeth s "length" []

cppRESERVE :: CompileInfo -> Int -> Cpp
cppRESERVE _ _ = CppNoop

cppSTACK :: Cpp
cppSTACK = CppIdent "g_vm->valstack"

cppCALLSTACK :: Cpp
cppCALLSTACK = CppIdent "g_vm->callstack"

cppARGSTACK :: Cpp
cppARGSTACK = CppIdent "g_vm->argstack"

cppSTACKBASE :: Cpp
cppSTACKBASE = CppIdent "g_vm->valstack_base"

cppSTACKTOP :: Cpp
cppSTACKTOP = CppIdent "g_vm->valstack_top"

cppOLDBASE :: Cpp
cppOLDBASE = CppIdent "oldbase"

cppMYOLDBASE :: Cpp
cppMYOLDBASE = CppIdent "myoldbase"

cppRET :: Cpp
cppRET = CppIdent "g_vm->ret"

cppLOC :: Int -> Cpp
cppLOC 0 = CppIndex cppSTACK cppSTACKBASE
cppLOC n = CppIndex cppSTACK (CppBinOp "+" cppSTACKBASE (CppNum (CppInt n)))

cppTOP :: Int -> Cpp
cppTOP 0 = CppIndex cppSTACK cppSTACKTOP
cppTOP n = CppIndex cppSTACK (CppBinOp "+" cppSTACKTOP (CppNum (CppInt n)))

cppPUSH :: [Cpp] -> Cpp
cppPUSH args = CppApp (CppProj cppCALLSTACK "push") args

cppPUSHARG :: [Cpp] -> Cpp
cppPUSHARG args = CppApp (CppProj cppARGSTACK "push") args

cppPOP :: Cpp
cppPOP = CppBinOp ";" (cppMeth cppCALLSTACK "top" []) (cppMeth cppCALLSTACK "pop" [])

cppPOPARGS :: Cpp
cppPOPARGS = CppBinOp ";" (cppMeth cppARGSTACK "top" []) (cppMeth cppARGSTACK "pop" [])

cppBOX :: Cpp -> Cpp
cppBOX obj = cppBOXTYPE obj (unboxedType obj)

cppUNBOXED :: Cpp -> String -> Cpp
cppUNBOXED obj typ = CppApp (CppIdent $ "unbox" ++ "<" ++ typ ++ ">") [obj]

unboxedType :: Cpp -> String
unboxedType e = case e of 
                  (CppString _)                       -> "string"
                  (CppNum (CppFloat _))               -> "double"
                  (CppNum (CppInteger (CppBigInt _))) -> "long long int"
                  (CppNum _)                          -> "int"
                  (CppChar _)                         -> "character"                          
                  _                                   -> ""

cppBOXTYPE :: Cpp -> String -> Cpp
cppBOXTYPE obj typ = case typ of
                       "" -> cppCall "box" [obj]
                       _  -> cppCall ("box" ++ "<" ++ typ ++ ">") [obj]

translateBC :: CompileInfo -> BC -> Cpp
translateBC info bc
  | ASSIGN r1 r2          <- bc = cppASSIGN info r1 r2
  | ASSIGNCONST r c       <- bc = cppASSIGNCONST info r c
  | UPDATE r1 r2          <- bc = cppASSIGN info r1 r2
  | ADDTOP n              <- bc = cppADDTOP info n
  | NULL r                <- bc = cppNULL info r
  | CALL n                <- bc = cppCALL info n
  | TAILCALL n            <- bc = cppTAILCALL info n
  | FOREIGNCALL r _ _ n a <- bc = cppFOREIGN info r n a
  | TOPBASE n             <- bc = cppTOPBASE info n
  | BASETOP n             <- bc = cppBASETOP info n
  | STOREOLD              <- bc = cppSTOREOLD info
  | SLIDE n               <- bc = cppSLIDE info n
  | REBASE                <- bc = cppREBASE info
  | RESERVE n             <- bc = cppRESERVE info n
  | MKCON r t rs          <- bc = cppMKCON info r t rs
  | CASE s r c d          <- bc = cppCASE info s r c d
  | CONSTCASE r c d       <- bc = cppCONSTCASE info r c d
  | PROJECT r l a         <- bc = cppPROJECT info r l a
  | OP r o a              <- bc = cppOP info r o a
  | ERROR e               <- bc = cppERROR info e
  | otherwise                   = CppRaw $ "//" ++ show bc
