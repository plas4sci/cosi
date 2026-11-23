{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}
module Lang.Frontend where

import Lang.Options
import Lang.Parser      (parseProgram)
import Lang.PrettyPrint (pprint)
import Lang.Semantics   (interpretWith, emptyEnv, bindHere, lookupBinding, Env, Value)
import Lang.Desugar     (desugar)
import Lang.ExtractPy   (extractProgram, extractModule)
import Lang.Syntax
import Lang.Types
import Lang.Primitives  (primitiveTypeConstructors)

import System.Directory (getCurrentDirectory, doesFileExist, canonicalizePath, listDirectory)
import System.Exit
import System.FilePath    ((</>), (<.>), takeExtension, replaceExtension, takeDirectory, takeFileName)
import qualified System.FilePath as FilePath

import Control.Monad (when, unless, forM, forM_)
import Control.Exception (try, IOException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, modify)
import qualified Data.Map as Map
import Data.List (isPrefixOf, nubBy, intercalate)
import Data.Function (on)

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
-- write out an equivalent Python program (and the modules it imports).
extractPy :: String -> [String] -> IO ()
extractPy fname rest = do
  result <- run True fname
  case result of
    Left _ -> exitFailure
    Right m -> do
      let outPath = case rest of
                      (out:_) -> out
                      []      -> replaceExtension fname ".py"
      written <- writePython m outPath
      mapM_ (\path -> putStrLn $ "Wrote Python translation to " <> path) written
      exitSuccess

-- | Write the Python translation of a module to the given path, and that of
-- each module it imports alongside it, returning the paths written.
writePython :: LoadedModule -> FilePath -> IO [FilePath]
writePython m outPath = do
  writeFile outPath (extractProgram (Just (modulePath m)) (moduleProgram m))
  depPaths <- forM (moduleDependencies m) $ \(name, dep) -> do
    let depPath = takeDirectory outPath </> name <.> "py"
    writeFile depPath (extractModule (modulePath dep) (moduleProgram dep))
    return depPath
  return (outPath : depPaths)

tryReadFile :: FilePath -> IO (Either IOException String)
tryReadFile path = try $ do
  !contents <- readFile path
  return contents

-- | Load (parse, check and evaluate) a cosi file and everything it imports,
-- reporting the outcome.
run :: Bool -> String -> IO (Either String LoadedModule)
run report fname0 = do
  fname <- resolveSourceFile fname0
  currentDir <- getCurrentDirectory
  let path = currentDir </> fname
  exists <- doesFileExist fname
  if not exists
    then do
      let alsoTried
            | takeExtension fname /= sourceExtension
                = " (also tried `" <> (fname <.> sourceExtension) <> "`)"
            | otherwise = ""
      putStrLn $ "File `" <> fname <> "` (Full path:" <> path <> ") cannot be found" <> alsoTried <> "."
      return $ Left "File not found"
    else do
      when report $ putStrLn $ "Checking " <> fname <> " (Full path:" <> path <> ") ..."
      result <- runExceptT (evalStateT (loadModule [] fname) Map.empty)
      case result of
        Left (LoadError kind msg) -> do
          let failure header = ansi_bold <> ansi_red <> header <> "\n" <> msg <> ansi_reset
          putStrLn $ case kind of
            LoadParseError    -> ansi_red ++ "Error: " ++ ansi_reset ++ msg
            LoadNotWellFormed -> failure "Not well-formed."
            LoadNotWellTyped  -> failure "Not well-typed."
            LoadImportError   -> failure "Import error."
          return $ Left msg
        Right m -> do
          putStrLn $ ansi_bold <> ansi_green
            <> "Well-typed " <> ansi_reset
            <> ansi_bold <> "as " <> ansi_reset <> pprint (moduleType m)
          return $ Right m

-- * Modules

-- | A cosi file that has been parsed, checked and evaluated
data LoadedModule = LoadedModule
  { modulePath     :: FilePath         -- ^ as given (relative to the working directory)
  , moduleProgram  :: Program 'Parsed
  , moduleOptions  :: [Option]
  , moduleType     :: Type 0
  , moduleValue    :: Value
  , moduleContext  :: Context          -- ^ top-level typing context, including imports
  , moduleEnv      :: Env              -- ^ top-level environment, including imports
  , moduleValues   :: Map.Map Identifier (Type 0, Value)
      -- ^ the values that can be imported from the module: those at its top
      -- level (including imports), except `it` (its result) and internal names
  , moduleDependencies :: [(Identifier, LoadedModule)]
      -- ^ every module imported, directly or indirectly, and its import name
  }

data LoadError = LoadError LoadErrorKind String

data LoadErrorKind = LoadParseError | LoadNotWellFormed | LoadNotWellTyped | LoadImportError

-- | Loading modules caches each loaded module by its canonical path, so a
-- module imported more than once is only loaded once
type Loader = StateT (Map.Map FilePath LoadedModule) (ExceptT LoadError IO)

-- | A chain of modules being loaded (canonical path, path): the last imported
-- by the one before it, and so on
type ImportChain = [(FilePath, FilePath)]

-- | Fail while loading the last module in the chain, noting the modules
-- through which it was imported
failIn :: ImportChain -> LoadErrorKind -> String -> Loader a
failIn chain kind msg = lift . throwE . LoadError kind $ msg <> importedVia
  where
    importers = map snd (take (length chain - 1) chain)
    importedVia | null importers = ""
                | otherwise      = "\n(imported via " <> intercalate " -> " importers <> ")"

-- | Load a module from an existing file, given the chain of modules importing it
loadModule :: ImportChain -> FilePath -> Loader LoadedModule
loadModule importers path = do
  key <- liftIO $ canonicalizePath path
  let chain = importers ++ [(key, path)]
  when (key `elem` map fst importers) $
    lift . throwE . LoadError LoadImportError $
      "import cycle: " <> intercalate " -> " (map snd (dropWhile ((/= key) . fst) chain))
  cached <- gets (Map.lookup key)
  case cached of
    Just m  -> return m
    Nothing -> do
      let ?srcFile = path
      let orFail :: LoadErrorKind -> (e -> String) -> Either e a -> Loader a
          orFail kind describe = either (failIn chain kind . describe) return
      contents <- liftIO (tryReadFile path) >>= orFail LoadImportError show
      (parsed, opts) <- orFail LoadParseError id (parseProgram path contents)

      -- Collect the type constructors declared by the module's classes
      -- TODO: bring them into scope for checking (and export them)
      openTypeConstructors <- orFail LoadNotWellTyped errorToString $
        buildTypes primitiveTypeConstructors parsed

      -- Imports: later imports shadow earlier ones
      imports <- mapM (loadImport chain path) [ spec | ImportDef spec <- parsed ]
      let importedValues = Map.unions (reverse [ vals | (_, _, vals) <- imports ])

      -- Desugar, check and evaluate, with the imported values in scope
      ast <- orFail LoadNotWellFormed errorToString (desugar parsed)
      (ctxt, ty) <- orFail LoadNotWellTyped errorToString $
        let ?typeConstructors = Map.union primitiveTypeConstructors openTypeConstructors
        in typeCheckProgram [ (x, t) | (x, (t, _)) <- Map.toList importedValues ] ast
      let env0 = Map.foldrWithKey (\x (_, v) -> bindHere x v) emptyEnv importedValues
          (env, value) = interpretWith env0 opts ast

      let m = LoadedModule
                { modulePath     = path
                , moduleProgram  = parsed
                , moduleOptions  = opts
                , moduleType     = ty
                , moduleValue    = value
                , moduleContext  = ctxt
                , moduleEnv      = env
                , moduleValues   = Map.fromList
                    [ (x, (t, v))
                    | (x, t) <- Map.toList (Map.fromListWith (\_new old -> old) ctxt)
                    , x /= "it", not ("%" `isPrefixOf` x)
                    , Right v <- [lookupBinding x env] ]
                , moduleDependencies = nubBy ((==) `on` (modulePath . snd))
                    (concat [ (name, dep) : moduleDependencies dep | (name, dep, _) <- imports ])
                }
      modify (Map.insert key m)
      return m

-- | Resolve and load an import made by the module at the end of the chain
-- (at the given path), and select the names it imports: returns the imported
-- module's name, the module, and the values imported from it
loadImport :: ImportChain -> FilePath -> ImportSpec
           -> Loader (Identifier, LoadedModule, Map.Map Identifier (Type 0, Value))
loadImport chain importer spec = do
  let name = case spec of
               ImportModule n -> n
               ImportAll n    -> n
               ImportOnly n _ -> n
      path = FilePath.normalise (takeDirectory importer </> name <.> sourceExtension)
      importError msg = failIn chain LoadImportError (importer <> ": " <> msg)
  -- The file name must match exactly, even on case-insensitive file systems
  -- (as in Python), so that a program resolves the same imports everywhere
  exists <- liftIO $ do
    fileExists <- doesFileExist path
    if fileExists
      then elem (takeFileName path) <$> listDirectory (takeDirectory path)
      else return False
  unless exists $
    importError $ "cannot find module `" <> name <> "` (looked for " <> path <> ")"

  m <- case spec of
    ImportModule _ ->
      importError $ "`import " <> name <> "` is not supported yet, as qualified names (`"
        <> name <> ".x`) are not available; use `from " <> name <> " import *` or `from "
        <> name <> " import x, y`"
    _ -> loadModule chain path
  let select keep = return (name, m, Map.filterWithKey (\x _ -> keep x) (moduleValues m))
  case spec of
    ImportOnly _ xs -> do
      forM_ xs $ \x ->
        unless (Map.member x (moduleValues m)) $
          importError $ "cannot import `" <> x <> "` from `" <> name <> "`: no such name"
      select (`elem` xs)
    -- `from m import *` brings in only public names, as in Python
    _ -> select (not . ("_" `isPrefixOf`))

ansi_red, ansi_green, ansi_reset, ansi_bold :: String
ansi_red   = "\ESC[31;1m"
ansi_green = "\ESC[32;1m"
ansi_reset = "\ESC[0m"
ansi_bold  = "\ESC[1m"
