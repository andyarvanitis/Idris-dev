module Idris.Output where

import Idris.Core.TT
import Idris.Core.Evaluate (isDConName, isTConName, isFnName)

import Idris.AbsSyntax
import Idris.Delaborate
import Idris.Docstrings
import Idris.IdeSlave

import Util.Pretty
import Util.ScreenSize (getScreenWidth)

import Debug.Trace

import System.IO (stdout, Handle, hPutStrLn)

import Data.Char (isAlpha)
import Data.List (nub)
import Data.Maybe (fromMaybe)

pshow :: IState -> Err -> String
pshow ist err = displayDecorated (consoleDecorate ist) .
                renderPretty 1.0 80 .
                fmap (fancifyAnnots ist) $ pprintErr ist err

iWarn :: FC -> Doc OutputAnnotation -> Idris ()
iWarn fc err =
  do i <- getIState
     case idris_outputmode i of
       RawOutput h ->
         do err' <- iRender . fmap (fancifyAnnots i) $
                      if fc_fname fc /= ""
                        then text (show fc) <> colon <//> err
                        else err
            runIO . hPutStrLn h $ displayDecorated (consoleDecorate i) err'
       IdeSlave n h ->
         do err' <- iRender . fmap (fancifyAnnots i) $ err
            let (str, spans) = displaySpans err'
            runIO . hPutStrLn h $
              convSExp "warning" (fc_fname fc, fc_start fc, fc_end fc, str, spans) n

iRender :: Doc a -> Idris (SimpleDoc a)
iRender d = do w <- getWidth
               ist <- getIState
               let ideSlave = case idris_outputmode ist of
                                IdeSlave _ _ -> True
                                _            -> False
               case w of
                 InfinitelyWide -> return $ renderPretty 1.0 1000000000 d
                 ColsWide n -> return $
                               if n < 1
                                 then renderPretty 1.0 1000000000 d
                                 else renderPretty 0.8 n d
                 AutomaticWidth | ideSlave  -> return $ renderPretty 1.0 1000000000 d
                                | otherwise -> do width <- runIO getScreenWidth
                                                  return $ renderPretty 0.8 width d

-- | Write a pretty-printed term to the console with semantic coloring
consoleDisplayAnnotated :: Handle -> Doc OutputAnnotation -> Idris ()
consoleDisplayAnnotated h output = do ist <- getIState
                                      rendered <- iRender $ output
                                      runIO . hPutStrLn h .
                                        displayDecorated (consoleDecorate ist) $
                                        rendered

iPrintTermWithType :: Doc OutputAnnotation -> Doc OutputAnnotation -> Idris ()
iPrintTermWithType tm ty = iRenderResult (tm <+> colon <+> align ty)

-- | Pretty-print a collection of overloadings to REPL or IDESlave - corresponds to :t name
iPrintFunTypes :: [(Name, Bool)] -> Name -> [(Name, PTerm)] -> Idris ()
iPrintFunTypes bnd n []        = iPrintError $ "No such variable " ++ show n
iPrintFunTypes bnd n overloads = do ist <- getIState
                                    let ppo = ppOptionIst ist
                                    let infixes = idris_infixes ist
                                    let output = vsep (map (uncurry (ppOverload ppo infixes)) overloads)
                                    iRenderResult output
  where fullName n = prettyName True True bnd n
        ppOverload ppo infixes n tm =
          fullName n <+> colon <+> align (pprintPTerm ppo bnd [] infixes tm)

iRenderOutput :: Doc OutputAnnotation -> Idris ()
iRenderOutput doc =
  do i <- getIState
     case idris_outputmode i of
       RawOutput h -> do out <- iRender doc
                         runIO $ putStrLn (displayDecorated (consoleDecorate i) out)
       IdeSlave n h ->
        do (str, spans) <- fmap displaySpans . iRender . fmap (fancifyAnnots i) $ doc
           let out = [toSExp str, toSExp spans]
           runIO . hPutStrLn h $ convSExp "write-decorated" out n

iRenderResult :: Doc OutputAnnotation -> Idris ()
iRenderResult d = do ist <- getIState
                     case idris_outputmode ist of
                       RawOutput h  -> consoleDisplayAnnotated h d
                       IdeSlave n h -> ideSlaveReturnAnnotated n h d

ideSlaveReturnWithStatus :: String -> Integer -> Handle -> Doc OutputAnnotation -> Idris ()
ideSlaveReturnWithStatus status n h out = do
  ist <- getIState
  (str, spans) <- fmap displaySpans .
                  iRender .
                  fmap (fancifyAnnots ist) $
                  out
  let good = [SymbolAtom status, toSExp str, toSExp spans]
  runIO . hPutStrLn h $ convSExp "return" good n


-- | Write pretty-printed output to IDESlave with semantic annotations
ideSlaveReturnAnnotated :: Integer -> Handle -> Doc OutputAnnotation -> Idris ()
ideSlaveReturnAnnotated = ideSlaveReturnWithStatus "ok"

-- | Show an error with semantic highlighting
iRenderError :: Doc OutputAnnotation -> Idris ()
iRenderError e = do ist <- getIState
                    case idris_outputmode ist of
                      RawOutput h  -> consoleDisplayAnnotated h e
                      IdeSlave n h -> ideSlaveReturnWithStatus "error" n h e

iPrintWithStatus :: String -> String -> Idris ()
iPrintWithStatus status s = do
  i <- getIState
  case idris_outputmode i of
    RawOutput h -> case s of
      "" -> return ()
      s  -> runIO $ hPutStrLn h s
    IdeSlave n h ->
      let good = SexpList [SymbolAtom status, toSExp s] in
      runIO $ hPutStrLn h $ convSExp "return" good n


iPrintResult :: String -> Idris ()
iPrintResult = iPrintWithStatus "ok"

iPrintError :: String -> Idris ()
iPrintError = iPrintWithStatus "error"

iputStrLn :: String -> Idris ()
iputStrLn s = do i <- getIState
                 case idris_outputmode i of
                   RawOutput h  -> runIO $ hPutStrLn h s
                   IdeSlave n h -> runIO . hPutStrLn h $ convSExp "write-string" s n


ideslavePutSExp :: SExpable a => String -> a -> Idris ()
ideslavePutSExp cmd info = do i <- getIState
                              case idris_outputmode i of
                                   IdeSlave n h -> runIO . hPutStrLn h $ convSExp cmd info n
                                   _ -> return ()

-- TODO: send structured output similar to the metavariable list
iputGoal :: SimpleDoc OutputAnnotation -> Idris ()
iputGoal g = do i <- getIState
                case idris_outputmode i of
                  RawOutput h -> runIO $ hPutStrLn h (displayDecorated (consoleDecorate i) g)
                  IdeSlave n h ->
                    let (str, spans) = displaySpans . fmap (fancifyAnnots i) $ g
                        goal = [toSExp str, toSExp spans]
                    in runIO . hPutStrLn h $ convSExp "write-goal" goal n

-- | Warn about totality problems without failing to compile
warnTotality :: Idris ()
warnTotality = do ist <- getIState
                  mapM_ (warn ist) (nub (idris_totcheckfail ist))
  where warn ist (fc, e) = iWarn fc (pprintErr ist (Msg e))


printUndefinedNames :: [Name] -> Doc OutputAnnotation
printUndefinedNames ns = text "Undefined " <> names <> text "."
  where names = encloseSep empty empty (char ',') $ map ppName ns
        ppName = prettyName True True []
