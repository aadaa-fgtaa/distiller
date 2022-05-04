module Main ( main ) where

import Distiller.Prelude
import Distiller.Name
import Distiller.Term
import Distiller.Term.Eval
import Distiller.Term.Parse
import Distiller.Term.Print
import Distiller.Transform
import System.Console.Repline
import System.IO.Utf8
import System.IO ( hPutStrLn )
import Data.String qualified as String

type ReplM = HaskelineT (FreshT (StateT (Maybe Prog) IO))

main :: IO ()
main = evaluatingStateT Nothing $ evalFreshT $ evalReplOpts ReplOpts
  { banner = const $ pure "> "
  , prefix = Just ':'
  , multilineCommand = Nothing
  , tabComplete = Combine File $ Word $ listWordCompleter $ fmap ((':':) . fst) opts
  , options = opts
  , command = const $ putTextLn "Unknown command. Type :? for help"
  , finaliser = pure Exit
  , initialiser = putTextLn "Type :? for help"
  }

opts :: Options ReplM
opts = fmap dontCrash <<$>>
  [ ("load", String.words >>> \case
      [path] -> parseFile path >>= \case
        Right prog -> put $ Just prog
        Left msg -> putStrLn msg >> put Nothing
      _ -> putStrLn "Invalid arguments. Type :? for help"
    )
  , ("save", String.words >>> \case
      [path] -> get >>= \case
        Just prog -> withFile path WriteMode (`hPrintPretty` prog)
        Nothing -> putStrLn "No program loaded"
      _ -> putStrLn "Invalid arguments. Type :? for help"
    )
  , ("print", String.words >>> \case
      [] -> get >>= \case
        Just prog -> printPretty prog
        Nothing -> putStrLn "No program loaded"
      _ -> putStrLn "Invalid arguments. Type :? for help"
    )
  , ("distill", String.words >>> \case
      [] -> get >>= \case
        Just prog -> put . Just =<< distillProg prog
        Nothing -> putStrLn "No program loaded"
      _ -> putStrLn "Invalid arguments. Type :? for help"
    )
  , ("transform", String.words >>> \case
      [readMaybe -> Just lvl] -> get >>= \case
        Just prog -> put . Just =<< transformProg lvl prog
        Nothing -> putStrLn "No program loaded"
      _ -> putStrLn "Invalid arguments. Type :? for help"
    )
  , ("eval", String.words >>> \case
      [] -> get >>= \case
        Just prog -> do
          (result, EvalStats{..}) <- eval prog
          printPretty result
          putStrLn $ "-- Substitutions: " <> show substitutions
          putStrLn $ "-- Case reductions: " <> show caseReductions
        Nothing -> putStrLn "No program loaded"
      [path] -> get >>= \case
        Just prog -> do
          (result, EvalStats{..}) <- eval prog
          withFile path WriteMode \h -> do
            hPrintPretty h result
            liftIO $ hPutStrLn h $ "-- Substitutions: " <> show substitutions
            liftIO $ hPutStrLn h $ "-- Case reductions: " <> show caseReductions
        Nothing -> putStrLn "No program loaded"
      _ -> putStrLn "Invalid arguments. Type :? for help"
    )
  , ("help", const printHelp)
  , ("?", const printHelp)
  ]

printHelp :: MonadIO m => m ()
printHelp = putText $ unlines
  [ "Available commands: "
  , ":load <filepath>   - load program from file"
  , ":save <filepath>   - save program to file"
  , ":print             - print current program"
  , ":distill           - perform distiallation on current program "
  , ":transform <level> - perform transformation of given level on current program "
  , ":eval              - evaluate current program "
  , ":eval <filepath>   - evaluate current program and write result to file"
  ]
