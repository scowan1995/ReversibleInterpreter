I want these language extensions for my syntactic sugaring tricks at the end

> {-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

I want my own definition of lookup and I want to write my own function
named "print".

> import Prelude hiding (lookup)

> import qualified Data.Map as Map
> import Data.Maybe

I want to get at the standard "print" function using the name System.print

> import qualified System.IO as System

I plan to use these monads to construct the parts of my interpreter

> import Control.Monad.Identity
> import Control.Monad.Except
> import Control.Monad.Reader
> import Control.Monad.State
> import Control.Monad.Writer
> import qualified Data.List as List

{-------------------------------------------------------------------}
{- The pure expression language                                    -}
{-------------------------------------------------------------------}

> data Val = I Int | B Bool
>            deriving (Eq, Show)

> data Expr = Const Val
>      | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
>      | And Expr Expr | Or Expr Expr | Not Expr
>      | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
>      | Var String
>    deriving (Eq, Show)

> type Name = String
> type Env = Map.Map Name Val

> lookup k t = case Map.lookup k t of
>                Just x -> return x
>                Nothing -> fail ("Unknown variable "++k)

{-- Monadic style expression evaluator,
 -- with error handling and Reader monad instance to carry dictionary
 --}

> type Eval a = ReaderT Env (ExceptT String Identity) a
> runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )

This evaluator could be a little neater

Integer typed expressions

> evali op e0 e1 = do e0' <- eval e0
>                     e1' <- eval e1
>                     case (e0', e1') of
>                          (I i0, I i1) -> return $ I (i0 `op` i1)
>                          _            -> fail "type error in arithmetic expression"

Boolean typed expressions

> evalb op e0 e1 = do e0' <- eval e0
>                     e1' <- eval e1
>                     case (e0', e1') of
>                          (B i0, B i1) -> return $ B (i0 `op` i1)
>                          _            -> fail "type error in boolean expression"

Operations over integers which produce booleans

> evalib op e0 e1 = do e0' <- eval e0
>                      e1' <- eval e1
>                      case (e0', e1') of
>                           (I i0, I i1) -> return $ B (i0 `op` i1)
>                           _            -> fail "type error in arithmetic expression"

Evaluate an expression

> eval :: Expr -> Eval Val
> eval (Const v) = return v
> eval (Add e0 e1) = do evali (+) e0 e1
> eval (Sub e0 e1) = do evali (-) e0 e1
> eval (Mul e0 e1) = do evali (*) e0 e1
> eval (Div e0 e1) = do evali div e0 e1

> eval (And e0 e1) = do evalb (&&) e0 e1
> eval (Or e0 e1) = do evalb (||) e0 e1

> eval (Not e0  ) = do evalb (const not) e0 (Const (B True))
>                        where not2 a _ = not a -- hack, hack

> eval (Eq e0 e1) = do evalib (==) e0 e1
> eval (Gt e0 e1) = do evalib (>) e0 e1
> eval (Lt e0 e1) = do evalib (<) e0 e1

> eval (Var s) = do env <- ask
>                   lookup s env


{-------------------------------------------------------------------}
{- The statement language                                          -}


> data Statement = Assign String Expr
>                | If Expr Statement Statement
>                | While Expr Statement
>                | Print Expr
>                | Seq Statement Statement
>                | Try Statement Statement
>                | Pass
>       deriving (Eq, Show)

type Eval a = ReaderT Env (ExceptT String Identity) a
type Env = Map.Map Name Val

> type History = Map.Map Name [Val]

> type MyType a = StateT (Env, History) (ExceptT String IO) a
> runMyType p = runExceptT (runStateT p (Map.empty, Map.empty))

> run1 :: Statement -> IO ()  --  written to take a mytype but says it takes a statement
> run1 prog = do
>   result <- runMyType $ exec prog
>   case result of
>     Left e -> System.print("Oh no an error in run1")
>     Right ((), env) -> return ()

> set :: (Name, Val) -> MyType ()
> set (s,i) = state (\table -> ((), (Map.insert s i (fst table), Map.insertWith (++) s [i] (snd table))))

> exec :: Statement -> MyType () -- Eval Val
> exec (Assign s e) = do
>    (env, his) <- get
>    Right res <- return $ runEval env (eval e)
>    set (s, res)

> exec (Seq e0 e1) = do
>    exec e0
>    liftIO $ putStrLn "enter s to get current state and then step, enter h to get current state, the history of all variables and then step, press anything else to just step"
>    c <- liftIO getChar
>    case c of
>      's' -> do (env, his) <- get
>                liftIO $ putStrLn "\n Showing state: "
>                liftIO $ putStrLn $ concat $ concatMap (\(a,b) -> [show a, " ", show b, "\n"]) $ Map.toList $ env
>                exec e1
>      'h' -> do (env, his) <- get
>                liftIO $ putStrLn "\n Showing state and history: "
>                liftIO $ putStrLn "\n current state: "
>                liftIO $ putStrLn $ concat $ concatMap (\(a,b) -> [show a, " ", show b, "\n"]) $ Map.toList $ env
>                liftIO $ putStrLn "\n history:"
>                liftIO $ putStrLn $ concat $ concatMap (\(a,b) -> [show a, " ", show b, "\n"]) $  Map.toList his
>                exec e1
>      _ -> exec e1

> exec (If cond s0 s1) = do (st, his) <-  get
>                           Right (B val) <- return $ runEval st (eval cond)
>                           if val then do exec s0 else do exec s1

> exec (While cond s) = do (st, his) <-  get
>                          Right (B val) <- return $ runEval st (eval cond)
>                          if val then do exec s >> exec (While cond s) else return ()

> exec (Try s0 s1) = do catchError (exec s0) (\e -> exec s1)

> exec _ = return ()
