{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DataKinds #-}
module Lang.REPL where

import Lang.Syntax
import Lang.Frontend    (banner, run, resolveSourceFile, ansi_bold, ansi_reset, LoadedModule(..))
import Lang.Parser      (parseExpr, parseType)
import Lang.PrettyPrint (pprint)
import Lang.Types       (synth, errorToString, Context)
import Lang.Semantics   (bigStep, Env, emptyEnv, Value)
import Lang.Kinding     (synthKind)
import Lang.Primitives  (TypeConstructors, primitiveTypeConstructors)
import Lang.Options     (Option)

import Data.Char          (isSpace)
import System.FilePath  (takeBaseName)
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)

data FileState = 
  FileState {
      filename :: FilePath
    , ast      :: Program 'Parsed
  }

data REPLState = 
  REPLState {
      currentFile :: Maybe FileState
    , prompt      :: String
    , env         :: Env
    , options     :: [Option]
    , typingContext :: Context
    , typeConstructors :: TypeConstructors
  }

initialState :: REPLState
initialState = REPLState {
     currentFile = Nothing
   , prompt      = ""
   , env         = emptyEnv
   , options     = []
   , typingContext = []
   , typeConstructors = primitiveTypeConstructors
  }

-- Main REPL
replLoop :: REPLState -> InputT IO REPLState
replLoop state = do
  line <- getInputLine (prompt state ++ "> ")
  case line of
    Nothing -> return state
    Just str -> case str of
      ':':rest ->
        -- Command
        case rest of
          'q':_ -> return state
          'h':_ -> do
            liftIO printHelp
            replLoop state
          'l':' ':path -> do
            state' <- liftIO $ loadFile state (trim path)
            replLoop state'

          'r':_ -> do
            case currentFile state of
              Nothing -> do
                liftIO $ putStrLn "No file loaded. Use :l <path> to load a file."
                replLoop state
              Just fs -> do
                runResult <- liftIO $ run False (filename fs)
                case runResult of
                  Left err -> do
                    liftIO $ putStrLn err
                    replLoop state
                  Right m -> do
                    liftIO $ displayResult (moduleValue m)
                    let fileState = FileState { filename = filename fs, ast = moduleProgram m }
                    replLoop $ state { currentFile = Just fileState
                                      , env = moduleEnv m
                                      , options = moduleOptions m
                                      , typingContext = moduleContext m
                                      , typeConstructors = moduleTypeConstructors m }

          't':' ':rest' -> do
            liftIO $ case parseExpr rest' of
              Left err  -> putStrLn err
              Right expr ->
                case let ?typeConstructors = typeConstructors state
                     in synth (typingContext state) expr of
                  Left err -> let ?srcFile = "<repl>" in putStrLn $ errorToString err
                  Right ty -> putStrLn $ pprint ty
            replLoop state

          'k':' ': rest' -> do
              liftIO $ case parseType rest' of
                  Left err -> putStrLn err
                  Right ty ->
                    case let ?typeConstructors = typeConstructors state
                         in synthKind ty of
                      Left err -> let ?srcFile = "<repl>" in putStrLn $ errorToString err
                      Right (ty', kind) -> do
                        if ty /= ty'
                          then putStrLn $ "Elaborated type: " ++ pprint ty'
                          else return ()
                        putStrLn $ pprint kind
              replLoop state
          rest' -> do
            liftIO $ putStrLn $ "Unknown command :" ++ rest'
            liftIO printHelp
            replLoop state
      -- not a command
      rest -> do
        liftIO $ case parseExpr rest of
          Left err   -> putStrLn err
          Right expr ->
            case bigStep (env state) (options state) expr of
              Left err  -> putStrLn err
              Right var -> putStrLn $ pprint var
        replLoop state

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse

displayResult :: Value -> IO ()
displayResult e = do
  putStrLn $ pprint e

-- | Load (type check and evaluate) a file into the REPL state. On failure the
-- error has already been printed by 'run' and the state is left unchanged.
loadFile :: REPLState -> FilePath -> IO REPLState
loadFile state path0 = do
  path <- resolveSourceFile path0
  runResult <- run False path
  case runResult of
    Left _ -> return state
    Right m -> do
      displayResult (moduleValue m)
      let fileState = FileState { filename = path, ast = moduleProgram m }
      return $ state { currentFile = Just fileState
                     , prompt = takeBaseName path
                     , env = moduleEnv m
                     , options = moduleOptions m
                     , typingContext = moduleContext m
                     , typeConstructors = moduleTypeConstructors m }

-- | Start the REPL, optionally loading a file first.
repl :: Maybe FilePath -> IO ()
repl mfile = do
  putStrLn $ ansi_bold <> banner <> ansi_reset
  putStrLn "Run :h for help"
  state <- maybe (return initialState) (loadFile initialState) mfile
  _ <- runInputT defaultSettings (replLoop state)
  return ()

printHelp :: IO ()
printHelp = do
  putStrLn "Commands:"
  putStrLn "    :h      - This message"
  putStrLn "    :t expr - Infer the type of an expression"
  putStrLn "    :k type - Infer the type of a type (its kind)"
  putStrLn "    :l path - Load the file"
  putStrLn "    :r      - Reload the currently loaded file"
  putStrLn "    :q      - Quit"
  putStrLn " Or type an expression to evaluate it"