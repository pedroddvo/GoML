{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module Transpile where

import Control.Monad (forM)
import Control.Monad.State (StateT (runStateT), evalStateT, MonadState (get), gets)
import Control.Monad.Except (ExceptT, runExceptT, MonadError (throwError))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Vector ( Vector )
import qualified Data.Vector as V

import Data.Map ( Map )
import qualified Data.Map as M

import Parse (Expr (..), parseExpr)
import Control.Monad.State (modify)
import Control.Monad (forM_)
import Debug.Trace (trace, traceM)


data Env = Env { envVars :: Map Text TypeId, envTypes :: Vector TypeInfo }
    deriving Show

type Trans = ExceptT Text (StateT Env IO)

type TypeId = Int
data TypeInfo = TIUnknown
              | TIRef TypeId
              | TIFunc [TypeId] TypeId
              | TINumber
    deriving Show

transpileTI :: TypeInfo -> Trans Text
transpileTI TIUnknown   = return T.empty
transpileTI TINumber    = return "int"
transpileTI (TIRef id)  = getType id >>= transpileTI

transpileTI (TIFunc ids ret) = do
    ids <- mapM (\id -> getType id >>= transpileTI) ids
           >>= return . T.intercalate ", "
    ret <- getType ret >>= transpileTI
    return $ T.concat [ "func(", ids, ") ", ret ]

getType :: TypeId -> Trans TypeInfo
getType id = gets envTypes >>= \v -> return $ (V.!) v id

assignType :: TypeInfo -> Trans TypeId
assignType t = do
    ts <- gets envTypes
    modify $ \s -> s { envTypes = V.snoc ts t }
    return $ V.length ts

assocType :: TypeId -> TypeInfo -> Trans ()
assocType id t = do
    ts <- gets envTypes
    modify $ \s -> s { envTypes = (V.//) ts [(id, t)] }

assignName :: TypeId -> Text -> Trans ()
assignName id n = do
    vars <- gets envVars
    modify $ \s -> s { envVars = M.insert n id vars }

unify :: TypeId -> TypeId -> Trans ()
unify at bt = do
    a <- getType at
    b <- getType bt
    case (a, b) of
        -- Follow references
        (TIRef at, _) -> unify at bt
        (_, TIRef bt) -> unify at bt

        (TINumber, TINumber) -> return ()

        -- Assume that they match
        (TIUnknown, _) -> assocType at (TIRef bt)
        (_, TIUnknown) -> assocType bt (TIRef at)

        (TIFunc aarg ares, TIFunc barg bres) -> do
            forM_ (zip aarg barg) (uncurry unify)
            unify ares bres
        
        (a', b') -> do
            a' <- transpileTI a'
            b' <- transpileTI b'
            throwError $ T.concat [ "Type mismatch between ", a', " and ", b' ]

--    name, args list, expr
-- -> function type id, argument type ids, return type id, expr text
transpileLet :: Text -> [Text] -> Expr -> Trans (TypeId, [TypeId], TypeId, Text)
transpileLet name args e = do
    argst   <- mapM (\_ -> assignType TIUnknown) args
    returnt <- assignType TIUnknown
    funct   <- assignType $ TIFunc argst returnt

    forM_ (zip argst args) (uncurry assignName)
    -- if no args, then variable is not a function, hence use its return type
    if null args
    then assignName returnt name
    else assignName funct name

    (e, t) <- transpile e
    -- return type of the function is just return type of the expr
    assocType returnt (TIRef t)

    return (funct, argst, returnt, e)



transpile :: Expr -> Trans (Text, TypeId)
transpile (MlNumber n) = assignType TINumber >>= curry return (T.pack $ show n)
transpile (MlSymbol s) = do
    v <- gets envVars
    case M.lookup s v of
        Nothing -> throwError $ T.concat [s, "is undefined!"]
        Just x -> do
            x <- assignType $ TIRef x
            return (s, x)

transpile (MlBinary o a b) = do
    (a, at) <- transpile a
    (b, bt) <- transpile b
    unify at bt
    return (T.concat [ a, o, b ], at)

transpile (MlLetBinding (MlLet name args e) rest) = do
    -- TODO: implement function, arguments
    (_, _, returnt, e) <- transpileLet name args e
    (rest, rt) <- transpile rest
    
    ret <- getType returnt >>= transpileTI
    return (T.concat ["var ", name, " ", ret, " = ", e, "\n", rest ], rt)

transpile (MlLet name args e) = do
    (funct, argst, returnt, e) <- transpileLet name args e

    let getTypeOrError (argt, arg) = do
        t <- getType argt
        case t of
            TIUnknown ->
                throwError $ T.concat [ "Type of ", arg, " could not be resolved in function ", name, "!" ]
            t' -> return t'
    
    args' <- mapM getTypeOrError (zip argst args)
             >>= mapM transpileTI
             >>= return . T.intercalate ", "
    ret   <- transpileTI =<< getType returnt

    return (T.concat [ "func ", name
                     , "(", args', ") ", ret, " {\n"
                     , e
                     , "}" ], funct)




transpileProgram :: [Expr] -> Trans Text
transpileProgram es = do
    es' <- forM es (fmap fst . transpile)
    return $ T.concat ("package main\n\n" : es')

testTranspile :: Text -> IO (Either (Text, Maybe Env) (Text, Env))
testTranspile s =
    case parseExpr s of
        Left err -> return $ Left (err, Nothing)
        Right es ->
            let t = transpileProgram es
            in do
                (x, env) <- runStateT (runExceptT t) (Env { envVars = M.empty, envTypes = V.empty })
                return $ case x of
                    Left err -> Left (err, Just env)
                    Right ex -> Right (ex, env)

testTranspileF :: String -> String -> IO ()
testTranspileF i o = do
  r <- T.readFile i
  t <- testTranspile r
  case t of
    Left (err, env)   -> T.putStrLn err >> print env
    Right (expr, env) -> T.writeFile o expr >> print env