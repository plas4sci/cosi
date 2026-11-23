{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}
module Lang.Frontend where

import Lang.Options
import Lang.Parser      (parseProgram)
import Lang.PrettyPrint (pprint)
import Lang.Semantics   (interpret, Env, Value)
import Lang.Desugar     (desugar)
import Lang.ExtractPy   (extractProgram)
import Lang.Syntax
import Lang.Types
import Lang.TypeError

import System.Directory (getCurrentDirectory, doesFileExist)
import System.Exit
import System.FilePath    ((</>), (<.>), takeExtension, replaceExtension)
import GHC.IO.Exception   (IOErrorType(..), ioe_type)

import Control.Monad (when)
import Control.Exception (try, IOException)

banner :: String
banner = "cosi v0.4.0 - Code with Scientific Intent"

-- | Resolve a source file name, allowing the @.cosi@ extension to be omitted:
-- if @fname@ does not exist but @fname.cosi@ does, use the latter.
resolveSourceFile :: FilePath -> IO FilePath
resolveSourceFile fname = do
  exists <- doesFileExist fname
  if exists || takeExtension fname == sourceExtension
    then return fname
    else do
      let withExt = fname <.> sourceExtension
      existsWithExt <- doesFileExist withExt
      return $ if existsWithExt then withExt else fname

sourceExtension :: String
sourceExtension = ".cosi"

-- | Parse, desugar and typecheck a cosi file, and if it is well-typed,
-- write out an equivalent Python program.
extractPy :: String -> [String] -> IO ()
extractPy fname rest = do
  result <- run True fname
  case result of
    Left _ -> exitFailure
    Right (parsetree, _, _, _, _) -> do
      let outPath = case rest of
                      (out:_) -> out
                      []      -> replaceExtension fname ".py"
      writeFile outPath (extractProgram (Just fname) parsetree)
      putStrLn $ "Wrote Python translation to " <> outPath
      exitSuccess

tryReadFile :: FilePath -> IO (Either IOException String)
tryReadFile path = try $ do
  !contents <- readFile path
  return contents

type RunResult = Either String (Program 'Parsed, [Option], Env, Value, Context)

-- | Parse, desugar, typecheck and evaluate a file, printing a (coloured)
-- summary of the outcome to stdout. If @report@ is set, also announce which
-- file is being checked.
run :: Bool -> String -> IO RunResult
run = runWith putStrLn

-- | As 'run', but print nothing: errors are only returned. Used by the test
-- suite, where the golden-file comparison reports the outcome instead.
runQuiet :: String -> IO RunResult
runQuiet = runWith (\_ -> return ()) False

-- | 'run' parameterised by how to emit its messages.
runWith :: (String -> IO ()) -> Bool -> String -> IO RunResult
runWith say report fname0 = do
  fname <- resolveSourceFile fname0
  currentDir <- getCurrentDirectory
  let path = currentDir </> fname
  result <- tryReadFile path
  case result of
    Right contents -> do
      when report $ say $ "Checking " <> fname <> " (Full path:" <> path <> ") ..."
      case parseProgram fname contents of
        Right (parsetree, options) -> do
          case desugar parsetree of
            Left err -> do
              let ?srcFile = fname
              say $ ansi_bold <> ansi_red
                      <> "Not well-formed.\n" <> errorToString err <> ansi_reset
              return $ Left (errorToString err)
            Right ast -> do
              -- Typing
              case typeCheck options ast of
                Left err -> do
                  let ?srcFile = fname
                  say $ ansi_bold <> ansi_red
                    <> "Not well-typed.\n" <> errorToString err <> ansi_reset
                  return $ Left (errorToString err)
                Right (ctxt, ty) -> do
                  -- Evaluate
                  let (env, normalForm) = interpret options ast

                  say $ ansi_bold <> ansi_green
                    <> "Well-typed " <> ansi_reset
                    <> ansi_bold <> "as " <> ansi_reset <> pprint ty

                  return $ Right (parsetree, options, env, normalForm, ctxt)
        Left msg -> do
          say $ ansi_red ++ "Error: " ++ ansi_reset ++ msg
          return $ Left msg
    Left e -> do
      case ioe_type e of
        NoSuchThing -> do
          let alsoTried
                | takeExtension fname /= sourceExtension
                    = " (also tried `" <> (fname <.> sourceExtension) <> "`)"
                | otherwise = ""
          say $ "File `" <> fname <> "` (Full path:" <> path <> ") cannot be found" <> alsoTried <> "."
          return $ Left "File not found"
        _ -> do
          let msg = show e
          say msg
          return $ Left msg

typeCheck :: [Option] -> Program 'Desugared -> Either TypeError (Context, Type 0)
typeCheck options program =
    case typeCheckProgram program of
        Right ty -> Right ty
        Left err -> Left err

ansi_red, ansi_green, ansi_reset, ansi_bold :: String
ansi_red   = "\ESC[31;1m"
ansi_green = "\ESC[32;1m"
ansi_reset = "\ESC[0m"
ansi_bold  = "\ESC[1m"
