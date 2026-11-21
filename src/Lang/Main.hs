module Lang.Main where

import Lang.Frontend    (banner, run, extractPy)
import Lang.PrettyPrint (pprint)
import Lang.REPL        (repl)

import System.Environment (getArgs)
import System.Exit

helpMessage :: String
helpMessage = unlines
  [ "Usage: cosi                          start the REPL"
  , "       cosi <filename>               run a file"
  , "       cosi -i <filename>            load a file into the REPL"
  , "       cosi <filename> --extract-py [output.py]"
  , "                                     translate a file to Python"
  , "       cosi --help"
  , ""
  , "The .cosi extension on <filename> may be omitted."
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl Nothing
    ("--help":_) -> putStrLn banner >> putStr helpMessage
    ["-i", fname] -> repl (Just fname)
    ("-i":_) -> do
      putStrLn "Usage: cosi -i <filename>"
      exitFailure
    -- `--extract-py` may appear anywhere among the arguments, e.g.
    -- `cosi --extract-py file.cosi` or `cosi file.cosi --extract-py`
    _ | "--extract-py" `elem` args ->
          case filter (/= "--extract-py") args of
            []           -> do
              putStrLn "Please supply a filename to extract"
              exitFailure
            (fname:rest) -> extractPy fname rest
    (fname:_) -> do
      putStrLn banner
      result <- run True fname
      case result of
        Left _   -> exitFailure
        Right (_, _, _, value, _)  -> do
          putStrLn $ pprint value
          exitSuccess
