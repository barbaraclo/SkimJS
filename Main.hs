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
--function
evalExpr env (FuncExpr Nothing args sttm) = return $ Function (Id "") args sttm
evalExpr env (FuncExpr (Just nome) args sttm) = return $ Function nome args sttm

-----------------------------------------------------
--Lista 
evalExpr env (ArrayLit []) = return (List [])
evalExpr env (ArrayLit (e:es)) = do
    exp1 <- evalExpr env e
    List expressoes <- evalExpr env (ArrayLit es)
    return (List (exp1:expressoes))
------------------------------------------------------
--chamada de função
evalExpr env (CallExpr expr expressoes) = do
   
    case expr of 
        (DotRef lista (Id "head")) -> do
            (List l) <- evalExpr env lista
            return $ head l

        (DotRef lista (Id "tail")) -> do
            (List l) <- evalExpr env lista
            return $ (List (tail l))
        (DotRef lista (Id "len")) -> do
             (List l) <- evalExpr env lista
             tamanhoLista (List l) (Int 0)
 --dois parametros       
        (DotRef lista (Id "concat")) -> do
            (List l) <- evalExpr env lista
            list <- evalExpr env (head expressoes)
            case list of
                (List a) -> return $ List (l++a)
                valor -> return $ List (l++[valor])
        
        (VarRef (Id name)) -> do
            f <- stateLookup env name 
            case f of 
              (Function id args sttm) -> do
                declareArgs env args expressoes
                evalStmt env (BlockStmt sttm)
--------------------------------------------------------
--Brackets
evalExpr env (BracketRef expr index) = do
    ex <- evalExpr env expr
    i <- evalExpr env index
    case ex of 
        (List t) -> do
            case i of 
                (Int j) -> getPosicao env ex i
                _ -> error $ "Index precisa ser inteiro"
        _ -> error $ "Objeto invalido"


--------------------------------------------------------
tamanhoLista :: Value -> Value -> StateTransformer Value
tamanhoLista (List []) (Int tam) = return $ Int tam
tamanhoLista (List (a:as)) (Int tam) = tamanhoLista (List as) (Int (tam+1))
------------------------------------------------------------------------------

getPosicao :: StateT -> Value -> Value -> StateTransformer Value
getPosicao env (List []) (Int index)  = error $ "Lista vazia"
getPosicao env (List (l:ls)) (Int index) = do
    if index < 0 then
        error $ "Index invalido"
    else
        if (index == 0) then 
            return l
        else
            getPosicao env (List ls) (Int (index - 1))
----------------------------------------------------------------------------
declareArgs :: StateT -> [Id] -> [Expression] -> StateTransformer Value
declareArgs env [] [] = return Nil
declareArgs env ((Id a):as) (p:ps) = do
    var <- evalExpr env p
    setVar a var
    declareArgs env as ps
---------------------------------------------------------------------------------

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
        _ -> do evalStmt env (BlockStmt stm)

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
--ForStmt
evalStmt env (ForStmt init test increment sttm) = do

    case init of 
        NoInit -> return Nil
        VarInit var -> evalStmt env (VarDeclStmt var)
        ExprInit expr -> evalExpr env expr
    case test of 
        Nothing -> do
            s <- evalStmt env sttm
            case s of
                Return t -> return (Return t)
                Break -> return Break
                _ -> do

                    case increment of 
                        Nothing -> evalStmt env (ForStmt NoInit test increment sttm)
                        Just i -> do
                                  evalExpr env i
                                  evalStmt env (ForStmt NoInit test increment sttm)

        Just t -> do
            Bool tst <- evalExpr env t 
            if tst then do
                s <- evalStmt env sttm
                case s of 
                     Return t -> return (Return t)
                     Break -> return Break
                     _ -> do
                        case increment of 
                           Nothing -> evalStmt env (ForStmt NoInit test increment sttm)
                           Just i -> do
                                    evalExpr env i
                                    evalStmt env (ForStmt NoInit test increment sttm)

            else return Nil

----------------------------------------------------

--Continue
evalStmt env (ContinueStmt continue) = return Continue
-----------------------------------------------------
--Break
evalStmt env (BreakStmt break) = return Break
-----------------------------------------------------
--SwitchCase 

evalStmt env (SwitchStmt expr []) = evalExpr env expr
evalStmt env (SwitchStmt expr (s:stm)) = do
    e <- evalExpr env expr
    case s of 
        CaseClause expressao arraysttm -> do 
            ex <- evalExpr env expressao
            Bool a <- infixOp env OpEq ex e
            case ex of 
                (List t) -> do 
                  if a then 
                    evalStmt env (BlockStmt arraysttm)
                  else
                    evalStmt env (SwitchStmt expr stm) 

                _ -> do 
                    if a then
                       evalStmt env (BlockStmt arraysttm)
                    else
                       evalStmt env (SwitchStmt expr stm) 
-----------------------------------------------------------
--Function
evalStmt env (FunctionStmt (Id name) args stmt) = setVar name (Function (Id name) args stmt)

-----------------------------------------------------------

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

-----------------------------------------------------------------
--String
infixOp env OpEq (String v1) (String v2) = return $ Bool $ v1 == v2
infixOp env OpAdd (String v1) (String v2) = return $ String $ v1++v2
-----------------------------------------------------------------
--Lista
infixOp env OpEq  (List [])  (List []) = return $ Bool True
infixOp env OpEq  (List [])  l  = return $ Bool False
infixOp env OpEq  l  (List [])  = return $ Bool False
infixOp env OpEq  (List l)   (List m) = do
    p <- infixOp env OpEq (head l) (head m)
    p1 <- infixOp env OpEq (List (tail l)) (List (tail m))
    Bool resul <- infixOp env OpLAnd p p1
    return $ Bool resul

infixOp env OpNEq  (List l) (List m) = do
    Bool resul <- infixOp env OpEq (List l)  (List m)
    return $ Bool  $ not resul

--infixOp env _ _ _ = return $ error "Operação inválida"
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
    js <- Parser.parseFromFile "teste1.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
