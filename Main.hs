import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    stateLookup env var -- crashes if the variable doesn't exist
    e <- evalExpr env expr
    setVar var e

evalExpr env (StringLit str) = return (String str)
-----------------------------------------------------
--Array 
evalExpr env (ArrayLit []) = return (Array [])
evalExpr env (ArrayLit (e:es)) = do
    exp1 <- evalExpr env e
    Array expressoes <- evalExpr env (ArrayLit es)
    return (Array (exp1:expressoes))
------------------------------------------------------
--chamada de função
evalExpr env (CallExpr expr expressoes) = do
    case expr of 
   
        (DotRef lista (Id "head")) -> do
            (Array l) <- evalExpr env lista
            return $ head l

        (DotRef lista (Id "tail")) -> do
            (Array l) <- evalExpr env lista
            return $ (Array (tail l))
        (DotRef lista (Id "len")) -> do
             (Array l) <- evalExpr env lista
             tamanhoArray (Array l) (Int 0)
 --dois parametros       
        (DotRef lista (Id "concat")) -> do
            (Array l) <- evalExpr env lista
            array <- evalExpr env (head expressoes)
            case array of
                (Array a) -> return $ Array (l++a)
                valor -> return $ Array (l++[valor])
        


--------------------------------------------------------
tamanhoArray :: Value -> Value -> StateTransformer Value
tamanhoArray (Array []) (Int tam) = return $ Int tam
tamanhoArray (Array (a:as)) (Int tam) = tamanhoArray (Array as) (Int (tam+1))
------------------------------------------------------------------------------


--TODO tail e concat 

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr
----------------------------------------------------
--return
evalStmt env (ReturnStmt expr) = 
    case expr of
        Nothing -> return Nil
        Just v -> do
            e <- evalExpr env v
            return $ Return e

----------------------------------------------------
--blockstm
evalStmt env (BlockStmt  []) = return Nil
evalStmt env (BlockStmt (s:stm)) = do 
    s1 <- evalStmt env s
    case s1 of 
        Break -> return Break 
        Return k -> return (Return k)
        _ -> do 
            evalStmt env s
            evalStmt env (BlockStmt stm)

-----------------------------------------------------
--IF sem else

evalStmt env (IfSingleStmt expr sttm) = do
    Bool e <- evalExpr env expr
    if e then do
        s <-evalStmt env sttm
        return s

    else
        return Nil
-- IF com else

evalStmt env (IfStmt expr sttmif sttmelse) = do 
        Bool e <- evalExpr env expr
        if e then do
            sif <- evalStmt env sttmif
            return sif
        else do
            selse <- evalStmt env sttmelse
            return selse
----------------------------------------------------
--DOwhile
evalStmt env (DoWhileStmt sttm expr) = do 
    s <- evalStmt env sttm 
    case s of 
        Break -> return Break
        _ -> do 
             Bool e <- evalExpr env expr
             if e then evalStmt env (DoWhileStmt sttm expr) else return Nil


-----------------------------------------------------
--while
evalStmt env (WhileStmt expr sttm) = do
       Bool e <- evalExpr env expr
       if e then do
        s <- evalStmt env sttm
        case s of 
            Break -> return Break
            _ -> evalStmt env (WhileStmt expr sttm)
        else
            return Nil
----------------------------------------------------
--Continue
evalStmt env (ContinueStmt continue) = return Continue
-----------------------------------------------------
--Break
evalStmt env (BreakStmt break) = return Break
-----------------------------------------------------
--SwitchCase

--evalStmt env (SwitchStmt expr []) = evalExpr env expr
--evalStmt env (SwitchStmt expr (s:stm)) = do
--    e <- evalExpr env expr
--    case s of 
--        CaseClause expressao arrsttm -> do 
--            ex <- evalExpr env expressao
--            Bool comp <- infixOp OpEq e expressao
--            if comp then do

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

infixOp env OpBAnd  (Int  v1) (Int  v2) = error $ "Não implementado"

--
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = Map.empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case Map.lookup var (union s env) of
        Nothing -> error $ "Variable " ++ show var ++ " not defiend."
        Just val -> (val, s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

--
-- Types and boilerplate
--

type StateT = Map String Value
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) =
    show val ++ "\n" ++ show (toList $ union defs environment) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f Map.empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "teste.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
