{-# LANGUAGE OverloadedStrings #-}
module Main where

import CEKMachine (Value(..), eval)
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Char8 qualified as BS
import System.Console.Haskeline
import Text.Trifecta (parseByteString, Result(..))
import TypeChecker (typeCheck)
import Parser (parseExp)
import System.Console.ANSI
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Prettyprinter hiding (Pretty)

-- | Custom pretty printing for values
class PrettyValue a where
  prettyValue :: a -> String

instance PrettyValue Value where
  prettyValue (VInt n) = show n
  prettyValue (VBool b) = show b
  prettyValue (VClos _ _) = "<closure>"

-- | REPL state
data ReplState = ReplState
  { lineCount :: Int
  }

initialState :: ReplState
initialState = ReplState { lineCount = 1 }

-- | Main REPL loop
main :: IO ()
main = do
  putStrLn "Welcome to Mini-ML REPL!"
  runInputT defaultSettings (loop initialState)
  where
    loop :: ReplState -> InputT IO ()
    loop state = do
      minput <- getInputLine (makePrompt state)
      case minput of
        Nothing -> outputStrLn "Goodbye!"
        Just ":q" -> outputStrLn "Goodbye!"
        Just input -> do
          when (not $ null input) $ do
            processInput input
          loop (state { lineCount = lineCount state + 1 })

    makePrompt :: ReplState -> String
    makePrompt state = "\ESC[1;32mλ " ++ show (lineCount state) ++ ">\ESC[0m "

-- | Process user input
processInput :: String -> InputT IO ()
processInput input = do
  case parseByteString parseExp mempty (BS.pack input) of
    Failure err -> do
      outputStrLn $ formatError "Parse error:"
      outputStrLn $ show err
    Success expr -> do
      -- Type checking
      case typeCheck expr of
        Left err -> do
          outputStrLn $ formatError "Type error:"
          outputStrLn err
        Right ty -> do
          outputStrLn $ formatType $ show ty
          -- Evaluation
          case eval expr of
            Nothing -> outputStrLn $ formatError "Evaluation error"
            Just val -> do
              outputStrLn $ formatValue $ prettyValue val

-- | Helper function to format error messages with colors
formatError :: String -> String
formatError msg = "\ESC[1;31m" ++ msg ++ "\ESC[0m"

-- | Helper function to format type information with colors
formatType :: String -> String
formatType ty = "\ESC[1;34mType:\ESC[0m " ++ ty

-- | Helper function to format values with colors
formatValue :: String -> String
formatValue val = "\ESC[1;32mValue:\ESC[0m " ++ val
