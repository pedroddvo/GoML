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

import Data.Set (Set)
import qualified Data.Set as S

import Parse (Expr (..), parseExpr)
import Control.Monad.State (modify)
import Control.Monad (forM_)
import Debug.Trace (trace, traceM)
import Control.Monad (when)




data Env = Env { envVars :: Map Text TypeId, envTypes :: Vector TypeInfo, envSigs :: Set Text }
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
getType id = gets envTypes >>= \v ->
    case (V.!) v id of
        (TIRef id) -> getType id
        e -> return e

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

getName :: Text -> Trans TypeId
getName s = do
    v <- gets envVars
    case M.lookup s v of
        Nothing -> throwError $ T.concat [s, " is undefined!"]
        Just x -> return x

unify :: TypeId -> TypeId -> Trans ()
unify at bt = do
    a <- getType at
    b <- getType bt
    case (a, b) of
        -- Follow references
        (TIRef at, _) -> unify at bt
        (_, TIRef bt) -> unify at bt

        (TINumber, TINumber) -> return ()

        (TIUnknown, TIUnknown) -> return ()

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
inferTypeLet :: Text -> [Text] -> Expr -> Trans (TypeId, [TypeId], TypeId)
inferTypeLet name args e = do
    argst   <- mapM (\_ -> assignType TIUnknown) args
    returnt <- assignType TIUnknown
    funct   <- assignType $ TIFunc argst returnt

    forM_ (zip argst args) (uncurry assignName)
    assignName funct name

    t <- inferType e
    -- return type of the function is just return type of the expr
    assocType returnt (TIRef t)

    return (funct, argst, returnt)

inferType :: Expr -> Trans TypeId
inferType (MlNumber n) = assignType TINumber
inferType (MlSymbol s) = getName s >>= return . TIRef >>= assignType

inferType (MlBinary o a b) = do
    at <- inferType a
    bt <- inferType b
    unify at bt
    return at

inferType (MlFunc args e) = do
    argst   <- mapM (\_ -> assignType TIUnknown) args
    returnt <- assignType TIUnknown
    funct   <- assignType $ TIFunc argst returnt

    forM_ (zip argst args) (uncurry assignName)

    t <- inferType e
    assocType returnt (TIRef t)

    return funct

inferType (MlSignature name args) = do
    let args' = reverse args
    argst   <- mapM (assignType . golangToTI) (reverse $ tail args')
    returnt <-      (assignType . golangToTI) (head args')
    funct   <- assignType (TIFunc argst returnt)

    assignName funct name
    ts <- gets envSigs
    modify $ \s -> s { envSigs = S.insert name ts }

    return funct


inferType (MlLetBinding (MlLet name args e) rest) = do
    -- TODO: implement function, arguments
    (_, _, _) <- inferTypeLet name args e
    rt <- inferType rest

    return rt

inferType (MlLet name args e) = do
    sigDefined <- gets envSigs >>= return . S.member name
    if sigDefined
    then do
        -- TODO: perhaps check return type too?
        (TIFunc dargs _) <- getName name >>= getType
        when (length args /= length dargs)
             (throwError $ T.concat [ "Wrong argument count in signature of ", name ])
        getName name
    else do
        alreadyDefined <- gets envVars >>= return . M.member name
        when alreadyDefined (throwError $ T.concat [ "Redefinition of ", name ])

        (_, _, returnt) <- inferTypeLet name args e

        return returnt


inferType (MlCurried ma b) = do
    at <- inferType ma
    at' <- getType at
    case at' of
        (TIFunc argst returnt) -> do
            when (null argst)
                 (throwError $ T.concat [ T.pack $ show ma, " took too many arguments!" ])
            arg <- getType (head argst)
            case arg of
                TIUnknown -> do
                    bt <- inferType b >>= getType
                    assocType (head argst) bt

                _ -> return ()

            func <- do
                if length argst == 1
                then getType returnt
                else return $ TIFunc (tail argst) returnt -- curry the function!

            assignType func

        _ -> throwError $ T.concat [ "Cannot curry a non-function: ", T.pack $ show ma ]


transpileCurry :: Maybe Text -> [(TypeId, Text)] -> TypeId -> Text -> Trans Text
transpileCurry _    []               returnt ex = return ex
transpileCurry name ((argt, arg):xs) returnt ex = do
    argt' <- getType argt >>= transpileTI
    rett  <- getType returnt >>= transpileTI

    rett  <- curryRet xs rett
    below <- transpileCurry Nothing xs returnt ex
    return $ T.concat [ "func", maybe "" (T.cons ' ') name, "(", arg, " ", argt', ")", rett
                      , "{\nreturn ", below, "\n}"]

    where
        -- Generates curried function signature
        -- e.g func(int) func(int) int
        curryRet [] returnt = return returnt
        curryRet ((argt, _):xs) returnt = do
            argt' <- getType argt >>= transpileTI

            below <- curryRet xs returnt
            return $ T.concat [ "func(", argt', ")", below ]


transpile :: Expr -> Trans Text
transpile (MlNumber n) = return (T.pack $ show n)
transpile (MlSymbol s) = return s

transpile (MlBinary o a b) = do
    a <- transpile a
    b <- transpile b

    return $ T.concat [ a, o, b ]

transpile (MlSignature _ _) = return ""

transpile e@(MlFunc args ex) = do
    (TIFunc argst returnt) <- inferType e >>= getType
    ex <- transpile ex

    transpileCurry (zip args argst) returnt ex
    where
        -- Generate curried function type
        -- e.g func(int) func(int) int
        ret [] r     = r
        ret (x:xs) r = T.concat [ "func(", x, ") ", ret xs r ]

        -- Generate curried anonymous function
        transpileCurry :: [(Text, TypeId)] -> TypeId -> Text -> Trans Text
        transpileCurry [] _ e = return e
        transpileCurry ((arg, argt):xs) returnt e = do
            argt'  <- getType argt >>= transpileTI
            xss    <- mapM (\(_, x) -> transpileTI =<< getType x) xs
            rcurry <- getType returnt >>= transpileTI >>= return . ret xss

            below <- transpileCurry xs returnt e
            return $ T.concat [ "func(",  arg, " ", argt', ") ", rcurry
                              , " {\n return ", below, "}"  ]


transpile e@(MlLetBinding (MlLet name _ ex) rest) = do
    -- TODO: implement function, arguments
    returnt <- inferType e
    ex      <- transpile ex
    rest    <- transpile rest

    ret <- getType returnt >>= transpileTI
    return $ T.concat [ "var ", name, " ", ret, " = ", ex, "\n", rest ]

transpile (MlLet name args ex) = do
    (TIFunc argst returnt) <- getName name >>= getType

    ex <- transpile ex

    if null args
    then return $ T.concat [ "func ", name, "() {\n", ex, "\n}" ]
    else transpileCurry (Just name) (zip argst args) returnt ex

transpile (MlCurried a b) = do
    a <- transpile a
    b <- transpile b

    return $ T.concat [ "(", a, ")(", b, ")" ]

    -- case at' of
    --     (TIFunc argst returnt) -> do
    --         (b, bt) <- transpile b

    --         -- TODO: future inferrence of function with unknown parameters
    --         --     | this can probably be implemented by splitting up type inference
    --         --     | and transpilation
    --         -- TODO: check for wrong arguments
    --         bt' <- getType bt
    --         arg <- getType (head argst)
    --         case arg of
    --             TIUnknown -> assocType (head argst) bt'

    --         -- TODO: i don't like this too much
    --         -- by transpiling ma again we can hack our way around future inferrence
    --         traceM $ show (a, at)

    --         func <- do
    --             if length argst == 1
    --             then getType returnt
    --             else return $ TIFunc (tail argst) returnt -- curry the function!
    --         funct <- assignType func

    --         return (T.concat [ "(", a, ")", "(", b, ")" ], funct)
    --     _ -> throwError $ T.concat [ "Cannot curry a non-function: ", a ]


-- let getTypeOrError (argt, arg) = do
--     t <- getType argt
--     case t of
--         TIUnknown ->
--             throwError $ T.concat [ "Type of ", arg, " could not be resolved in function ", name, "!" ]
--         t' -> return t'


transpileProgram :: [Expr] -> Trans Text
transpileProgram es = do
    forM_ es inferType -- Infer whole tree
    es' <- forM es (\e -> transpile e >>= \t -> return $ T.concat [t, "\n\n"])
    return $ T.concat ("package main\n\n" : es')

testTranspile :: Text -> IO (Either (Text, Maybe Env) (Text, Env))
testTranspile s =
    case parseExpr s of
        Left err -> return $ Left (err, Nothing)
        Right es ->
            let t = transpileProgram es
            in do
                (x, env) <- runStateT (runExceptT t) (Env { envVars = M.empty, envTypes = V.empty, envSigs = S.empty })
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