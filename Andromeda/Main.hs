module Main where

import System.Console.Haskeline
import qualified Andromeda.Input as In
import Andromeda.Desugar
import Andromeda.Typing
import Andromeda.Parser
import Andromeda.Norm
import Andromeda.Context
import Andromeda.Print
import Andromeda.Common
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Applicative
import Control.Monad.Except
import Options.Applicative hiding (runParser)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

data Options = Options
               { notInteractive :: Bool
               , files :: [String] }

options :: Parser Options
options = Options
          <$> switch (short 'n' <> help "Don't run interactive toplevel")
          <*> many (argument str (metavar "FILES..."))


main :: IO ()
main = execParser optionParser >>= run
  where optionParser = info (helper <*> options) fullDesc

run :: Options -> IO ()
run o = do c <- foldM evalFile emptyContext (files o)
           unless (notInteractive o) (repl c)

repl :: Context -> IO ()
repl ctx = runInputT defaultSettings (loop ctx)
  where
    loop c = do
      line <- getInputLine "> "
      case line of
        Nothing -> return ()
        Just "Quit." -> return ()
        Just s -> liftIO (eval c "repl" (L.pack s)) >>= loop

evalFile :: Context -> FilePath -> IO Context
evalFile ctx path = liftIO $ L.readFile path >>= eval ctx path

eval :: Context -> FilePath -> L.Text -> IO Context
eval ctx path code = do
  r <- runExceptT (parse path code >>= foldM evalExpr ctx)
  case r of
    Left e -> do putStrLn $ printError e
                 return ctx
    Right c -> return c

evalExpr :: Context -> In.Directive -> ErrorT IO Context
evalExpr ctx (e', loc) =
  case e' of
    In.Eval e -> do
      e1 <- desugar ctx e
      t <- infer ctx e1
      e2 <- nf ctx e1
      liftIO $ putStrLn $ printExpr ctx e2 ++ " : " ++ printExpr ctx t
      return ctx
    In.Context -> do
      liftIO $ putStrLn $ printContext ctx
      return ctx
    In.Parameter x t -> do
      u <- desugar ctx t
      _ <- inferUniverse ctx u
      return $ addParameter x u ctx
    In.Definition x e ->
      if elem x (contextNames ctx) then
        typeError loc (x ++ " already exists")
      else do
        e1 <- desugar ctx e
        t <- infer ctx e1
        return $ addDefinition x t e1 ctx
    In.Check e -> do
      e1 <- desugar ctx e
      t <- infer ctx e1
      liftIO $ putStrLn $ printExpr ctx e1 ++ " : " ++ printExpr ctx t
      return ctx
    In.Help -> return ctx
