{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Transpile where

import Parse (Expr (..), parseExpr)

import Data.Text ( Text )
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Control.Monad.Writer ( WriterT, MonadWriter (tell), execWriterT )
import Control.Monad (forM_)

type Trans = WriterT Text IO

tellS :: [Text] -> Trans ()
tellS ts = forM_ ts tell

transpile :: Expr -> Trans ()
transpile (MlNumber n) = tell (T.pack . show $ n)
transpile (MlSymbol s) = tell s

transpile (MlBinary o a b) = transpile a *> tell o *> transpile b

transpile (MlLet name args expr) = do
    tellS [ "func ", name, "() {\n "]
    transpile expr
    tell "\n}"

transpile (MlLetBinding (MlLet name args expr) rest) = do
    tellS [ name, " := " ] *> transpile expr *> tell "\n"
    transpile rest



transpileProgram :: [Expr] -> Trans ()
transpileProgram es = do
    tell "package main\n\n"
    forM_ es (\e -> transpile e <* tell "\n")

testTranspile :: Text -> IO (Either Text Text)
testTranspile s = 
    case parseExpr s of
        Left err -> return $ Left err
        Right es -> let t = transpileProgram es
                    in do
                        e <- execWriterT t
                        return $ Right e

testTranspileF :: String -> String -> IO ()
testTranspileF i o = do
    r <- T.readFile i
    t <- testTranspile r
    case t of
        Left e  -> T.putStrLn e
        Right e -> T.writeFile o e