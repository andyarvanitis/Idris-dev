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
import qualified Text.Printf as PF


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
     String ->          -- extra object files`as
     String ->          -- libraries
     String ->          -- extra compiler flags
     DbgLevel ->        -- debug level
     IO ()

codegenCpp_all definitions outputType filename includes objs libs flags dbg = do
  let bytecode = map toBC definitions
  let decls = concatMap toDecl (map fst bytecode)
  -- let info = initCompileInfo bytecode
  let cpp = concatMap (toCpp (CompileInfo [] [] True)) bytecode
  -- let cpp = concatMap (translateDecl info) bytecode
  -- let full = concatMap processFunction cpp
  --let code = deadCodeElim full
  -- let (cons, opt) = optimizeConstructors full
  let (header, rt) = ("", "")
  
  path       <- (++) <$> getDataDir <*> (pure "/cpprts/")
                
  let cppout = (  T.pack (headers includes)
                  `T.append` namespaceBegin
                  `T.append` T.pack decls
                  -- `T.append` T.concat (map varDecl opt)
                  -- `T.append` T.pack "\n\n"
                  -- `T.append` T.concat (map varDecl cons)
                  `T.append` T.pack "\n\n"
                  -- `T.append` T.concat (map compileCpp opt)
                  -- `T.append` T.concat (map compileCpp cons)
                  `T.append` T.concat (map compileCpp cpp)
                  `T.append` namespaceEnd
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
                    " -I " ++ path ++
                    " -I. " ++ objs ++ " -x c++ " ++
                    (if (outputType == Executable) then "" else " -c ") ++
                    " " ++ tmpn ++
                    " " ++ libStandard ++ " " ++
                    " -L " ++ path ++
                    " " ++ libRuntime ++ " " ++
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
                             (xs ++ ["idris_runtime.h"])

      debug TRACE = "#define IDRIS_TRACE\n\n"
      debug _ = ""

      -- We're using signed integers now. Make sure we get consistent semantics
      -- out of them from cc. See e.g. http://thiemonagel.de/2010/01/signed-integer-overflow/
      ccFlags = " -fwrapv -fno-strict-overflow"

      ccStandard = "-std=c++11"
      libStandard = "-lc++"
      libRuntime = "-lidris_cpp_rts"

      ccDbg DEBUG = "-g"
      ccDbg TRACE = "-O2"
      ccDbg _ = "-O2"      

      -- varDecl :: Cpp -> T.Text
      -- varDecl (CppAlloc name (Just (CppFunction _ _))) = T.pack $ "void " ++ name ++ "(" ++ (intercalate "," cppFUNCPARMS) ++ ");\n"
      -- varDecl (CppAlloc name _) = T.pack $ "extern Value " ++ name ++ ";\n"
      -- varDecl _ = ""


      toDecl :: Name -> String
      toDecl f = "void " ++ translateName f ++ "(" ++ (intercalate ", " cppFUNCPARMS) ++ ");\n"

      -- deadCodeElim :: [Cpp] -> [Cpp]
      -- deadCodeElim cpp = concatMap collectFunctions cpp
      --   where
      --     collectFunctions :: Cpp -> [Cpp]
      --     collectFunctions fun@(CppAlloc name _)
      --       | name == translateName (sMN 0 "runMain") = [fun]
      --
      --     collectFunctions fun@(CppAlloc name (Just (CppFunction _ body))) =
      --       let invokations = sum $ map (
      --               \x -> execState (countInvokations name x) 0
      --             ) cpp
      --        in if invokations == 0
      --              then []
      --              else [fun]
      --
      --     countInvokations :: String -> Cpp -> State Int ()
      --     countInvokations name (CppAlloc _ (Just (CppFunction _ body))) =
      --       countInvokations name body
      --
      --     countInvokations name (CppSeq seq) =
      --       void $ traverse (countInvokations name) seq
      --
      --     countInvokations name (CppAssign _ rhs) =
      --       countInvokations name rhs
      --
      --     countInvokations name (CppCond conds) =
      --       void $ traverse (
      --           runKleisli $ arr id *** Kleisli (countInvokations name)
      --         ) conds
      --
      --     countInvokations name (CppSwitch _ conds def) =
      --       void $ traverse (
      --         runKleisli $ arr id *** Kleisli (countInvokations name)
      --       ) conds >> traverse (countInvokations name) def
      --
      --     countInvokations name (CppApp lhs rhs) =
      --       void $ countInvokations name lhs >> traverse (countInvokations name) rhs
      --
      --     countInvokations name (CppNew _ args) =
      --       void $ traverse (countInvokations name) args
      --
      --     countInvokations name (CppArray args) =
      --       void $ traverse (countInvokations name) args
      --
      --     countInvokations name (CppIdent name')
      --       | name == name' = get >>= put . (+1)
      --       | otherwise     = return ()
      --
      --     countInvokations _ _ = return ()

      -- processFunction :: Cpp -> [Cpp]
      -- processFunction =
      --   collectSplitFunctions . (\x -> evalRWS (splitFunction x) () 0)

      namespaceBegin :: T.Text
      namespaceBegin = T.pack "namespace idris {\n"

      namespaceEnd :: T.Text
      namespaceEnd = T.pack "} // namespace idris\n"

-- optimizeConstructors :: [Cpp] -> ([Cpp], [Cpp])
-- optimizeConstructors cpp =
--     let (cpp', cons) = runState (traverse optimizeConstructor' cpp) M.empty in
--         (map (allocCon . snd) (M.toList cons), cpp')
--   where
--     allocCon :: (String, Cpp) -> Cpp
--     allocCon (name, con) = CppAlloc name (Just con)
--
--     newConstructor :: Int -> String
--     newConstructor n = "_con_" ++ show n
--
--     optimizeConstructor' :: Cpp -> State (M.Map Int (String, Cpp)) Cpp
--     optimizeConstructor' cpp@(CppNew "_con_" [ CppNum (CppInt tag)
--                                            , CppArray []
--                                            , a
--                                            , e
--                                            ]) = do
--       s <- get
--       case M.lookup tag s of
--            Just (i, c) -> return $ CppIdent i
--            Nothing     -> do let n = newConstructor tag
--                              put $ M.insert tag (n, cpp) s
--                              return $ CppIdent n
--
--     optimizeConstructor' (CppSeq seq) =
--       CppSeq <$> traverse optimizeConstructor' seq
--
--     optimizeConstructor' (CppSwitch reg cond def) = do
--       cond' <- traverse (runKleisli $ arr id *** Kleisli optimizeConstructor') cond
--       def'  <- traverse optimizeConstructor' def
--       return $ CppSwitch reg cond' def'
--
--     optimizeConstructor' (CppCond cond) =
--       CppCond <$> traverse (runKleisli $ arr id *** Kleisli optimizeConstructor') cond
--
--     optimizeConstructor' (CppAlloc fun (Just (CppFunction args body))) = do
--       body' <- optimizeConstructor' body
--       return $ CppAlloc fun (Just (CppFunction args body'))
--
--     optimizeConstructor' (CppAssign lhs rhs) = do
--       lhs' <- optimizeConstructor' lhs
--       rhs' <- optimizeConstructor' rhs
--       return $ CppAssign lhs' rhs'
--
--     optimizeConstructor' cpp = return cpp

-- collectSplitFunctions :: (Cpp, [(Int,Cpp)]) -> [Cpp]
-- collectSplitFunctions (fun, splits) = map generateSplitFunction splits ++ [fun]
--   where
--     generateSplitFunction :: (Int,Cpp) -> Cpp
--     generateSplitFunction (depth, CppAlloc name fun) =
--       CppAlloc (name ++ "_" ++ show depth) fun

-- splitFunction :: Cpp -> RWS () [(Int,Cpp)] Int Cpp
-- splitFunction (CppAlloc name (Just (CppFunction args body@(CppSeq _)))) = do
--   body' <- splitSequence body
--   return $ CppAlloc name (Just (CppFunction args body'))
--     where
--       splitCondition :: Cpp -> RWS () [(Int,Cpp)] Int Cpp
--       splitCondition cpp
--         | CppCond branches <- cpp =
--             CppCond <$> processBranches branches
--         | CppSwitch cond branches def <- cpp =
--             CppSwitch cond <$> (processBranches branches) <*> (traverse splitSequence def)
--         | otherwise = return cpp
--         where
--           processBranches :: [(Cpp,Cpp)] -> RWS () [(Int,Cpp)] Int [(Cpp,Cpp)]
--           processBranches =
--             traverse (runKleisli (arr id *** Kleisli splitSequence))
--
--       splitSequence :: Cpp -> RWS () [(Int, Cpp)] Int Cpp
--       splitSequence cpp@(CppSeq seq) =
--         let (pre,post) = break isCall seq in
--             case post of
--                  []                    -> CppSeq <$> traverse splitCondition seq
--                  [cpp@(CppCond _)]       -> splitCondition cpp
--                  [cpp@(CppSwitch {})] -> splitCondition cpp
--                  [_]                   -> return cpp
--                  (call:rest) -> do
--                    depth <- get
--                    put (depth + 1)
--                    new <- splitFunction (newFun rest)
--                    tell [(depth, new)]
--                    return $ CppSeq (pre ++ (newCall depth : [call]))
--
--       splitSequence cpp = return cpp
--
--       isCall :: Cpp -> Bool
--       isCall (CppApp (CppIdent "vmcall") _) = True
--       isCall _                            = False
--
--       newCall :: Int -> Cpp
--       newCall depth =
--         CppApp (CppIdent "vmcall") [ cppVM
--                                    , CppIdent $ name ++ "_" ++ show depth
--                                    , CppArray [cppOLDBASE, cppMYOLDBASE]
--                                    ]
--
--       newFun :: [Cpp] -> Cpp
--       newFun seq =
--         CppAlloc name (Just $ CppFunction cppFUNCPARMS (CppSeq seq))
--
-- splitFunction cpp = return cpp

-- translateDecl :: CompileInfo -> (Name, [BC]) -> [Cpp]
-- translateDecl info (name@(MN 0 fun), bc)
--   | txt "APPLY" == fun || txt "EVAL" == fun =
--          allocCaseFunctions (snd body)
--       ++ [ CppAlloc (
--                translateName name
--            ) (Just $ CppFunction cppFUNCPARMS (
--                CppSeq $ map (translateBC info) (fst body) ++ [
--                  CppCond [ ( (translateReg $ caseReg (snd body)) `cppInstanceOf` "C" `cppAnd` func
--                           , CppApp func [cppVM, cppOLDBASE, cppMYOLDBASE]
--                           )
--                           , ( CppNoop
--                             , CppSeq $ map (translateBC info) (defaultCase (snd body))
--                             )
--                         ]
--                ]
--              )
--            )
--          ]
--   where
--     func = CppProj (cppUNBOX cppCON $ translateReg $ caseReg $ snd body) "function"
--
--     body :: ([BC], [BC])
--     body = break isCase bc
--
--     isCase :: BC -> Bool
--     isCase bc
--       | CASE {} <- bc = True
--       | otherwise          = False
--
--     defaultCase :: [BC] -> [BC]
--     defaultCase ((CASE _ _ _ (Just d)):_) = d
--
--     caseReg :: [BC] -> Reg
--     caseReg ((CASE _ r _ _):_) = r
--
--     allocCaseFunctions :: [BC] -> [Cpp]
--     allocCaseFunctions ((CASE _ _ c _):_) = splitBranches c
--     allocCaseFunctions _                  = []
--
--     splitBranches :: [(Int, [BC])] -> [Cpp]
--     splitBranches = map prepBranch
--
--     prepBranch :: (Int, [BC]) -> Cpp
--     prepBranch (tag, code) =
--       CppAlloc (
--         translateName name ++ "_" ++ show tag
--       ) (Just $ CppFunction cppFUNCPARMS (
--           CppSeq $ map (translateBC info) code
--         )
--       )
--
-- translateDecl info (name, bc) =
--   [ CppAlloc (
--        translateName name
--      ) (Just $ CppFunction cppFUNCPARMS (
--          CppSeq $ map (translateBC info)bc
--        )
--      )
--   ]

-- toCpp :: Name -> [BC] -> String
-- toCpp name bc =
toCpp info (name, bc) =
  [ CppIdent $ "void " ++ translateName name,
    CppFunction cppFUNCPARMS (
      CppSeq $ CppAlloc (Just $ cppBASETYPENAME ++ " __unused") cppMYOLDBASENAME Nothing : map (translateBC info)bc
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
translateChar ch
  | isAscii ch && isAlphaNum ch  = [ch]
  | ch `elem` [' ','_', ',','.'] = [ch]
  | otherwise                    = cppCodepoint (ord ch)

translateName :: Name -> String
translateName n = "_idris_" ++ concatMap cchar (showCG n)
  where cchar x | isAlphaNum x = [x]
                | otherwise    = "_" ++ show (fromEnum x) ++ "_"

cppASSIGN :: CompileInfo -> Reg -> Reg -> Cpp
cppASSIGN _ r1 r2 = CppAssign (translateReg r1) (translateReg r2)

cppASSIGNCONST :: CompileInfo -> Reg -> Const -> Cpp
cppASSIGNCONST _ r c = CppAssign (translateReg r) (cppBOX' $ translateConstant c)

cppCALL :: CompileInfo -> Name -> Cpp
cppCALL _ n =
  CppApp (
    CppIdent "vm_call"
  ) [cppVM, CppIdent (translateName n), cppMYOLDBASE]

cppTAILCALL :: CompileInfo -> Name -> Cpp
cppTAILCALL _ n =
  CppApp (
    CppIdent "vm_tailcall"
  ) [cppVM, CppIdent (translateName n), cppOLDBASE]

cppFOREIGN :: CompileInfo -> Reg -> String -> [(FType, Reg)] -> FType -> Cpp
cppFOREIGN _ reg n args ret
  | n == "fileOpen"
  , [(_, name),(_, mode)] <- args =
      CppAssign (
        translateReg reg
      ) (
        cppBOX cppManagedPtr $ cppCall "fileOpen" [cppUNBOX cppSTRING $ translateReg name,
                                                   cppUNBOX cppSTRING $ translateReg mode]
      )

  | n == "fileClose"
  , [(_, fh)] <- args =
      CppAssign (
        translateReg reg
      ) (
        cppCall "fileClose" [cppUNBOX cppManagedPtr $ translateReg fh]
      )

  | n == "fputStr"
  , [(_, fh),(_, str)] <- args =
      CppAssign (
        translateReg reg
      ) (
        cppCall "fputStr" [cppUNBOX cppManagedPtr $ translateReg fh, 
                           cppUNBOX cppSTRING $ translateReg str]
      )

  | n == "fileEOF"
  , [(_, fh)] <- args =
      CppAssign (
        translateReg reg
      ) (
        cppBOX cppINT $ cppCall "fileEOF" [cppUNBOX cppManagedPtr $ translateReg fh]
      )

  | n == "fileError"
  , [(_, fh)] <- args =
      CppAssign (
        translateReg reg
      ) (
        cppBOX cppINT $ cppCall "fileError" [cppUNBOX cppManagedPtr $ translateReg fh]
      )

  | n == "isNull"
  , [(_, arg)] <- args =
      CppAssign (
        translateReg reg
      ) (
        cppBOX cppBOOL $ CppBinOp "==" (translateReg arg) CppNull
      )

  | n == "idris_eqPtr"
  , [(_, lhs),(_, rhs)] <- args =
      CppAssign (
        translateReg reg
      ) (
        CppBinOp "==" (translateReg lhs) (translateReg rhs)
      )

  | n == "getenv"
  , [(_, arg)] <- args =
      CppAssign (
        translateReg reg
      ) (
        cppBOX cppSTRING $ cppCall "getenv" [cppMeth (cppUNBOX cppSTRING $ translateReg arg) "c_str" []]
      )

  | otherwise =
     CppAssign (
       translateReg reg
     ) (
       let callexpr = CppFFI n (map generateWrapper args) in
       case ret of
         FUnit -> CppBinOp "," CppNull callexpr
         _     -> cppBOX (T.unpack . compileCpp $ foreignToBoxed ret) $ callexpr
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
        cppUNBOX (T.unpack . compileCpp $ foreignToBoxed ty) $ translateReg reg
        
      cType :: FType -> Cpp
      cType (FArith (ATInt ITNative))       = CppIdent "int"
      cType (FArith (ATInt ITChar))         = CppIdent "char"
      cType (FArith (ATInt ITBig))          = CppIdent "long long"
      cType (FArith (ATInt (ITFixed IT8)))  = CppIdent "int8_t"
      cType (FArith (ATInt (ITFixed IT16))) = CppIdent "int16_t"
      cType (FArith (ATInt (ITFixed IT32))) = CppIdent "int32_t"
      cType (FArith (ATInt (ITFixed IT64))) = CppIdent "int64_t"
      cType FString = CppIdent "string"
      cType FUnit = CppIdent "void"
      cType FPtr = CppIdent "void*"
      cType FManagedPtr = CppIdent "shared_ptr<void>"
      cType (FArith ATFloat) = CppIdent "double"
      cType FAny = CppIdent "void*"
      cType (FFunction a b) = CppList [cType a, cType b]

      foreignToBoxed :: FType -> Cpp
      foreignToBoxed (FArith (ATInt ITNative))       = CppIdent cppINT
      foreignToBoxed (FArith (ATInt ITChar))         = CppIdent cppCHAR
      foreignToBoxed (FArith (ATInt ITBig))          = CppIdent cppBIGINT
      foreignToBoxed (FArith (ATInt (ITFixed IT8)))  = CppIdent (cppWORD 8)
      foreignToBoxed (FArith (ATInt (ITFixed IT16))) = CppIdent (cppWORD 16)
      foreignToBoxed (FArith (ATInt (ITFixed IT32))) = CppIdent (cppWORD 32)
      foreignToBoxed (FArith (ATInt (ITFixed IT64))) = CppIdent (cppWORD 64)
      foreignToBoxed FString = CppIdent cppSTRING
      -- foreignToBoxed FUnit = CppIdent "void"
      foreignToBoxed FPtr = CppIdent cppPTR
      foreignToBoxed FManagedPtr = CppIdent cppManagedPtr
      foreignToBoxed (FArith ATFloat) = CppIdent cppFLOAT
      foreignToBoxed FAny = CppIdent cppPTR
      -- foreignToBoxed (FFunction a b) = CppList [cType a, cType b]

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
cppSLIDE _ n = CppApp (CppIdent "slide") [cppVM, CppNum (CppInt n)]

cppMKCON :: CompileInfo -> Reg -> Int -> [Reg] -> Cpp
cppMKCON info r t rs =
  CppAssign (translateReg r) (
    cppBOX cppCON $ CppList $ CppNum (CppInt t) : args rs
  ) 
    where
      args [] = []        
      args xs = [CppList (map translateReg xs)]

      -- func :: Int -> [Cpp]
      -- func n
      --   | n `elem` compileInfoApplyCases info = [CppIdent $ translateName (sMN 0 "APPLY") ++ "_" ++ show n]
      --   | n `elem` compileInfoEvalCases info  = [CppIdent $ translateName (sMN 0 "EVAL") ++ "_" ++ show n]
      --   | otherwise = []

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

      cppTAG cpp =
        (CppTernary (cpp `cppInstanceOf` "C") (
          CppProj (cppUNBOX cppCON cpp) "tag"
        ) (CppNum (CppInt $ negate 1)))

      cppCTAG :: Cpp -> Cpp
      cppCTAG cpp = CppProj (cppUNBOX cppCON cpp) "tag"


cppCONSTCASE :: CompileInfo -> Reg -> [(Const, [BC])] -> Maybe [BC] -> Cpp
cppCONSTCASE info reg cases def =
  CppCond $ (
    map (unboxedBinOp (cppEq) (translateReg reg) . translateConstant *** prepBranch) cases
  ) ++ (maybe [] ((:[]) . ((,) CppNoop) . prepBranch) def)
    where
      prepBranch :: [BC] -> Cpp
      prepBranch bc = CppSeq $ map (translateBC info) bc

      unboxedBinOp :: (Cpp -> Cpp -> Cpp) -> Cpp -> Cpp -> Cpp
      unboxedBinOp f l r = f (cppUNBOX (unboxedType r) l) r
      
cppPROJECT :: CompileInfo -> Reg -> Int -> Int -> Cpp
cppPROJECT _ reg loc 0  = CppNoop
cppPROJECT _ reg loc 1  =
  CppAssign (cppLOC loc) (
    CppIndex (CppProj (cppUNBOX cppCON $ translateReg reg) "args")
             (CppNum $ CppInt 0)
  )
cppPROJECT _ reg loc ar =
  CppApp (CppIdent "project") [ cppVM
                              , translateReg reg
                              , CppNum (CppInt loc)
                              , CppNum (CppInt ar)
                              ]

cppOP :: CompileInfo -> Reg -> PrimFn -> [Reg] -> Cpp
cppOP _ reg oper args = CppAssign (translateReg reg) (cppOP' oper)
  where
    cppOP' :: PrimFn -> Cpp
    cppOP' op
      | LNoOp <- op = translateReg (last args)

      | (LZExt ty ITNative) <- op = cppBOX cppINT    $ cppUNBOX (cppAType (ATInt ty)) $ translateReg (last args)
      | (LZExt ty ITBig)    <- op = cppBOX cppBIGINT $ cppUNBOX (cppAType (ATInt ty)) $ translateReg (last args)

      | (LPlus ty)  <- op
      , (lhs:rhs:_) <- args = cppBOX (cppAType ty) $ CppBinOp "+" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                                  (cppUNBOX (cppAType ty) $ translateReg rhs)
      | (LMinus ty)  <- op
      , (lhs:rhs:_)  <- args = cppBOX (cppAType ty) $ CppBinOp "-" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                                   (cppUNBOX (cppAType ty) $ translateReg rhs)
      | (LTimes ty)  <- op
      , (lhs:rhs:_)  <- args = cppBOX (cppAType ty) $ CppBinOp "*" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                                   (cppUNBOX (cppAType ty) $ translateReg rhs)
      | (LSDiv ty)  <- op
      , (lhs:rhs:_) <- args = cppBOX (cppAType ty) $ CppBinOp "/" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                                  (cppUNBOX (cppAType ty) $ translateReg rhs)
      | (LSRem ty)  <- op
      , (lhs:rhs:_) <- args = cppBOX (cppAType ty) $ CppBinOp "%" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                                  (cppUNBOX (cppAType ty) $ translateReg rhs)
      | (LEq ty)    <- op
      , (lhs:rhs:_) <- args = cppBOX cppBOOL $ CppBinOp "==" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                             (cppUNBOX (cppAType ty) $ translateReg rhs)
      | (LSLt ty)   <- op
      , (lhs:rhs:_) <- args = cppBOX cppBOOL $ CppBinOp "<"  (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                             (cppUNBOX (cppAType ty) $ translateReg rhs)
      | (LSLe ty)   <- op
      , (lhs:rhs:_) <- args = cppBOX cppBOOL $ CppBinOp "<=" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                             (cppUNBOX (cppAType ty) $ translateReg rhs)
      | (LSGt ty)   <- op
      , (lhs:rhs:_) <- args = cppBOX cppBOOL $ CppBinOp ">"  (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                             (cppUNBOX (cppAType ty) $ translateReg rhs)
      | (LSGe ty)   <- op
      , (lhs:rhs:_) <- args = cppBOX cppBOOL $ CppBinOp ">=" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                             (cppUNBOX (cppAType ty) $ translateReg rhs)
      -- | (LPlus (ATInt ITChar)) <- op
      -- , (lhs:rhs:_)            <- args =
      --     cppCall "fromCharCode" [
      --       CppBinOp "+" (
      --         cppCall "charCode" [translateReg lhs]
      --       ) (
      --         cppCall "charCode" [translateReg rhs]
      --       )
      --     ]

      | (LTrunc ITNative (ITFixed IT8)) <- op
      , (arg:_)                               <- args =
          cppBOX (cppWORD 8) $ CppBinOp "&" (cppUNBOX cppINT $ translateReg arg) (CppRaw "0xFFu")

      | (LTrunc (ITFixed IT16) (ITFixed IT8)) <- op
      , (arg:_)                               <- args =
          cppBOX (cppWORD 8) $ CppBinOp "&" (cppUNBOX (cppWORD 16) $ translateReg arg) (CppRaw "0xFFu")

      | (LTrunc (ITFixed IT32) (ITFixed IT16)) <- op
      , (arg:_)                                <- args =
          cppBOX (cppWORD 16) $ CppBinOp "&" (cppUNBOX (cppWORD 32) $ translateReg arg) (CppRaw "0xFFFFu")

      | (LTrunc (ITFixed IT64) (ITFixed IT32)) <- op
      , (arg:_)                                <- args =
          cppBOX (cppWORD 32) $ CppBinOp "&" (cppUNBOX (cppWORD 64) $ translateReg arg) (CppRaw "0xFFFFFFFFu")

      | (LTrunc ITBig (ITFixed IT64)) <- op
      , (arg:_)                       <- args =
          cppBOX (cppWORD 64) $ CppBinOp "&" (cppUNBOX cppBIGINT $ translateReg arg) (CppRaw "0xFFFFFFFFFFFFFFFFu")

      | (LTrunc ITBig ITNative) <- op
      , (arg:_)                 <- args = cppBOX cppINT $ cppStaticCast (cppINT ++ "::type") (cppUNBOX cppBIGINT $ translateReg arg)


      | (LLSHR ty@(ITFixed _)) <- op
      , (lhs:rhs:_)           <- args = cppOP' (LASHR ty)

      | (LLt ty@(ITFixed _)) <- op
      , (lhs:rhs:_)         <- args = cppOP' (LSLt (ATInt ty))

      | (LLe ty@(ITFixed _)) <- op
      , (lhs:rhs:_)         <- args = cppOP' (LSLe (ATInt ty))

      | (LGt ty@(ITFixed _)) <- op
      , (lhs:rhs:_)         <- args = cppOP' (LSGt (ATInt ty))

      | (LGe ty@(ITFixed _)) <- op
      , (lhs:rhs:_)         <- args = cppOP' (LSGe (ATInt ty))

      | (LUDiv ty@(ITFixed _)) <- op
      , (lhs:rhs:_)           <- args = cppOP' (LSDiv (ATInt ty))

      | (LAnd ty)    <- op
      , (lhs:rhs:_) <- args = cppBOX (cppAType (ATInt ty)) $ CppBinOp "&" (cppUNBOX (cppAType (ATInt ty)) $ translateReg lhs)
                                                                          (cppUNBOX (cppAType (ATInt ty)) $ translateReg rhs)
      | (LOr ty)     <- op
      , (lhs:rhs:_) <- args = cppBOX (cppAType (ATInt ty)) $ CppBinOp "|" (cppUNBOX (cppAType (ATInt ty)) $ translateReg lhs)
                                                                          (cppUNBOX (cppAType (ATInt ty)) $ translateReg rhs)
      | (LXOr ty)    <- op
      , (lhs:rhs:_) <- args = cppBOX (cppAType (ATInt ty)) $ CppBinOp "^" (cppUNBOX (cppAType (ATInt ty)) $ translateReg lhs)
                                                                          (cppUNBOX (cppAType (ATInt ty)) $ translateReg rhs)
      | (LSHL ty)    <- op
      , (lhs:rhs:_) <- args = cppBOX (cppAType (ATInt ty)) $ CppBinOp "<<" (cppUNBOX (cppAType (ATInt ty)) $ translateReg lhs)
                                                                           (cppAsIntegral $ translateReg rhs)
      | (LASHR ty)   <- op
      , (lhs:rhs:_) <- args = cppBOX (cppAType (ATInt ty)) $ CppBinOp ">>" (cppUNBOX (cppAType (ATInt ty)) $ translateReg lhs)
                                                                           (cppAsIntegral $ translateReg rhs)
      | (LCompl ty)  <- op
      , (arg:_)     <- args = cppBOX (cppAType (ATInt ty)) $ CppPreOp "~" (cppUNBOX (cppAType (ATInt ty)) $ translateReg arg)

      | LStrConcat  <- op
      , (lhs:rhs:_) <- args = cppBOX cppSTRING $ CppBinOp "+" (cppUNBOX cppSTRING $ translateReg lhs)
                                                              (cppUNBOX cppSTRING $ translateReg rhs)
      | LStrEq      <- op
      , (lhs:rhs:_) <- args = cppBOX cppBOOL $ CppBinOp "==" (cppUNBOX cppSTRING $ translateReg lhs)
                                                             (cppUNBOX cppSTRING $ translateReg rhs)
      | LStrLt      <- op
      , (lhs:rhs:_) <- args = cppBOX cppBOOL $ CppBinOp "<"  (cppUNBOX cppSTRING $ translateReg lhs)
                                                             (cppUNBOX cppSTRING $ translateReg rhs)
      | LStrLen     <- op
      , (arg:_)     <- args = cppBOX cppINT $ cppStaticCast (cppINT ++ "::type") (strLen (cppUNBOX cppSTRING $ translateReg arg)) -- TODO: int size 64?

      | (LStrInt ITNative)      <- op
      , (arg:_)                 <- args = cppBOX cppINT $ cppCall "stoi" [cppUNBOX cppSTRING $ translateReg arg]

      | (LIntStr ITNative)      <- op
      , (arg:_)                 <- args = cppBOX cppSTRING $ cppAsString $ translateReg arg

      | (LSExt ITNative ITBig)  <- op
      , (arg:_)                 <- args = cppBOX cppBIGINT $ cppUNBOX cppINT $ translateReg arg

      | (LIntStr ITBig)         <- op
      , (arg:_)                 <- args = cppBOX cppSTRING $ cppAsString $ translateReg arg

      | (LStrInt ITBig)         <- op
      , (arg:_)                 <- args = cppBOX cppBIGINT $ cppCall "stoll" [cppUNBOX cppSTRING $ translateReg arg]

      | LFloatStr               <- op
      , (arg:_)                 <- args = cppBOX cppSTRING $ cppAsString $ translateReg arg

      | LStrFloat               <- op
      , (arg:_)                 <- args = cppBOX cppFLOAT $ cppCall "stod" [cppUNBOX cppSTRING $ translateReg arg]

      | (LIntFloat ITNative)    <- op
      , (arg:_)                 <- args = cppBOX cppFLOAT $ cppUNBOX cppINT $ translateReg arg

      | (LFloatInt ITNative)    <- op
      , (arg:_)                 <- args = cppBOX cppINT $ cppUNBOX cppFLOAT $ translateReg arg

      | (LChInt ITNative)       <- op
      , (arg:_)                 <- args = cppBOX cppINT $ cppUNBOX cppCHAR $ translateReg arg

      | (LIntCh ITNative)       <- op
      , (arg:_)                 <- args = cppBOX cppCHAR $ cppUNBOX cppINT $ translateReg arg

      | LFExp       <- op
      , (arg:_)     <- args = cppBOX cppFLOAT $ cppCall "exp" [cppUNBOX cppFLOAT $ translateReg arg]
      | LFLog       <- op
      , (arg:_)     <- args = cppBOX cppFLOAT $ cppCall "log" [cppUNBOX cppFLOAT $ translateReg arg]
      | LFSin       <- op
      , (arg:_)     <- args = cppBOX cppFLOAT $ cppCall "sin" [cppUNBOX cppFLOAT $ translateReg arg]
      | LFCos       <- op
      , (arg:_)     <- args = cppBOX cppFLOAT $ cppCall "cos" [cppUNBOX cppFLOAT $ translateReg arg]
      | LFTan       <- op
      , (arg:_)     <- args = cppBOX cppFLOAT $ cppCall "tan" [cppUNBOX cppFLOAT $ translateReg arg]
      | LFASin      <- op
      , (arg:_)     <- args = cppBOX cppFLOAT $ cppCall "asin" [cppUNBOX cppFLOAT $ translateReg arg]
      | LFACos      <- op
      , (arg:_)     <- args = cppBOX cppFLOAT $ cppCall "acos" [cppUNBOX cppFLOAT $ translateReg arg]
      | LFATan      <- op
      , (arg:_)     <- args = cppBOX cppFLOAT $ cppCall "atan" [cppUNBOX cppFLOAT $ translateReg arg]
      | LFSqrt      <- op
      , (arg:_)     <- args = cppBOX cppFLOAT $ cppCall "sqrt" [cppUNBOX cppFLOAT $ translateReg arg]
      | LFFloor     <- op
      , (arg:_)     <- args = cppBOX cppFLOAT $ cppCall "floor" [cppUNBOX cppFLOAT $ translateReg arg]
      | LFCeil      <- op
      , (arg:_)     <- args = cppBOX cppFLOAT $ cppCall "ceil" [cppUNBOX cppFLOAT $ translateReg arg]

      | LStrCons    <- op
      , (lhs:rhs:_) <- args = cppBOX cppSTRING $ CppBinOp "+" (cppAsString $ translateReg lhs)
                                                              (cppUNBOX cppSTRING $ translateReg rhs)
      | LStrHead    <- op
      , (arg:_)     <- args = 
          let str = cppUNBOX cppSTRING $ translateReg arg in      
              CppTernary (cppAnd (translateReg arg) (CppPreOp "!" (cppMeth str "empty" [])))
                         (cppBOX cppCHAR $ cppCall "utf8_head" [str])
                         CppNull
                         --(cppBOX cppCHAR $ CppChar (cppCodepoint 0))

      | LStrRev     <- op
      , (arg:_)     <- args = cppBOX cppSTRING $ cppCall "reverse" [cppUNBOX cppSTRING $ translateReg arg]

      | LStrIndex   <- op
      , (lhs:rhs:_) <- args = cppBOX cppCHAR $ cppCall "char32_from_utf8_string" [cppUNBOX cppSTRING $ translateReg lhs, 
                                                                                  cppAsIntegral $ translateReg rhs]
      | LStrTail    <- op
      , (arg:_)     <- args =
          let str = cppUNBOX cppSTRING $ translateReg arg in
              CppTernary (cppAnd (translateReg arg) (cppGreaterThan (strLen str) cppOne))
                         (cppBOX cppSTRING $ cppCall "utf8_tail" [str])
                         (cppBOX cppSTRING $ CppString "")

      | LReadStr    <- op
      , (arg:_)     <- args = cppBOX cppSTRING $ cppCall "freadStr" [cppUNBOX cppManagedPtr $ translateReg arg]

      | LSystemInfo <- op
      , (arg:_) <- args = cppBOX cppSTRING $ cppCall "systemInfo"  [translateReg arg]

      | LNullPtr    <- op
      , (_)         <- args = CppNull

      | otherwise = CppError $ "Not implemented: " ++ show op
        where
          invokeMeth :: Reg -> String -> [Reg] -> Cpp
          invokeMeth obj meth args =
            CppApp (CppProj (translateReg obj) meth) $ map translateReg args
            
          strLen :: Cpp -> Cpp
          strLen s = cppMeth s "length" []

cppRESERVE :: CompileInfo -> Int -> Cpp
cppRESERVE _ _ = CppNoop

cppSTACK :: Cpp
cppSTACK = CppIdent "vm->valstack"

cppCALLSTACK :: Cpp
cppCALLSTACK = CppIdent "vm->callstack"

cppARGSTACK :: Cpp
cppARGSTACK = CppIdent "vm->argstack"

cppSTACKBASE :: Cpp
cppSTACKBASE = CppIdent "vm->valstack_base"

cppSTACKTOP :: Cpp
cppSTACKTOP = CppIdent "vm->valstack_top"

cppBASETYPENAME :: String
cppBASETYPENAME = "IndexType"

cppOLDBASENAME :: String
cppOLDBASENAME = "oldbase"

cppOLDBASE :: Cpp
cppOLDBASE = CppIdent cppOLDBASENAME

cppMYOLDBASENAME :: String
cppMYOLDBASENAME = "myoldbase"

cppMYOLDBASE :: Cpp
cppMYOLDBASE = CppIdent cppMYOLDBASENAME

cppVM :: Cpp
cppVM = CppIdent "vm"

cppFUNCPARMS :: [String]
cppFUNCPARMS = ["shared_ptr<VirtualMachine>& vm", cppBASETYPENAME ++ " " ++ cppOLDBASENAME]

cppRET :: Cpp
cppRET = CppIdent "vm->ret"

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

cppBOX' :: Cpp -> Cpp
cppBOX' obj = cppBOX (unboxedType obj) obj

cppUNBOX :: String -> Cpp -> Cpp
cppUNBOX typ obj = CppApp (CppIdent $ "unbox" ++ "<" ++ typ ++ ">") [obj]

unboxedType :: Cpp -> String
unboxedType e = case e of 
                  (CppString _)                       -> cppSTRING
                  (CppNum (CppFloat _))               -> cppFLOAT
                  (CppNum (CppInteger (CppBigInt _))) -> cppBIGINT
                  (CppNum _)                          -> cppINT
                  (CppChar _)                         -> cppCHAR
                  (CppWord (CppWord8 _))              -> cppWORD 8
                  (CppWord (CppWord16 _))             -> cppWORD 16
                  (CppWord (CppWord32 _))             -> cppWORD 32
                  (CppWord (CppWord64 _))             -> cppWORD 64
                  _                                   -> ""

cppBOX :: String -> Cpp -> Cpp
cppBOX typ obj = case typ of
                       "" -> cppCall "box" [obj]
                       _  -> cppCall ("box" ++ "<" ++ typ ++ ">") [obj]

cppAsString :: Cpp -> Cpp
cppAsString obj = cppPtrMeth obj "asString" []

cppAsIntegral :: Cpp -> Cpp
cppAsIntegral obj = cppPtrMeth obj "asIntegral" []

cppINT :: String
cppINT = "Int"

cppBIGINT :: String
cppBIGINT = "BigInt"

cppFLOAT :: String
cppFLOAT = "Float"

cppSTRING :: String
cppSTRING = "String"

cppCHAR :: String
cppCHAR = "Char"

cppWORD :: Int -> String
cppWORD n = PF.printf "Word%d" n

cppManagedPtr :: String
cppManagedPtr = "ManagedPtr"

cppPTR :: String
cppPTR = "Ptr"

cppCON :: String
cppCON = "Con"

cppBOOL :: String
cppBOOL = cppINT

cppAType :: ArithTy -> String
cppAType (ATInt ITNative)       = cppINT
cppAType (ATInt ITBig)          = cppBIGINT
cppAType (ATInt ITChar)         = cppCHAR
cppAType (ATFloat)              = cppFLOAT
cppAType (ATInt (ITFixed IT8))  = cppWORD 8
cppAType (ATInt (ITFixed IT16)) = cppWORD 16
cppAType (ATInt (ITFixed IT32)) = cppWORD 32
cppAType (ATInt (ITFixed IT64)) = cppWORD 64
cppAType (ty)                   = "UNKNOWN TYPE: " ++ show ty


cppCodepoint :: Int -> String
cppCodepoint c = PF.printf "\\U%.8X" c


translateBC :: CompileInfo -> BC -> Cpp
translateBC info bc
  | ASSIGN r1 r2          <- bc = cppASSIGN info r1 r2
  | ASSIGNCONST r c       <- bc = cppASSIGNCONST info r c
  | UPDATE r1 r2          <- bc = cppASSIGN info r1 r2
  | ADDTOP n              <- bc = cppADDTOP info n
  | NULL r                <- bc = cppNULL info r
  | CALL n                <- bc = cppCALL info n
  | TAILCALL n            <- bc = cppTAILCALL info n
  | FOREIGNCALL r _ t n a <- bc = cppFOREIGN info r n a t
  | TOPBASE n             <- bc = cppTOPBASE info n
  | BASETOP n             <- bc = cppBASETOP info n
  | STOREOLD              <- bc = cppSTOREOLD info
  | SLIDE n               <- bc = cppSLIDE info n
  | REBASE                <- bc = cppREBASE info
  | RESERVE n             <- bc = cppRESERVE info n
  | MKCON r _ t rs        <- bc = cppMKCON info r t rs
  | CASE s r c d          <- bc = cppCASE info s r c d
  | CONSTCASE r c d       <- bc = cppCONSTCASE info r c d
  | PROJECT r l a         <- bc = cppPROJECT info r l a
  | OP r o a              <- bc = cppOP info r o a
  | ERROR e               <- bc = cppERROR info e
  | otherwise                   = CppRaw $ "//" ++ show bc

