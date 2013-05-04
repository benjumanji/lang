module Main
  where

import Control.Applicative (liftA2)
import Control.Monad.Trans.Reader
import Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)


data Fix f = In { out :: f (Fix f) }

data ExprF r = 
      I Int
    | Add r r
    | Mul r r
    | Let String r r
    | Var String


type Expr = Fix ExprF

i :: Int -> Expr 
i x = In (I x)

add :: Expr -> Expr -> Expr
add x y = In (Add x y)

mul :: Expr -> Expr -> Expr
mul x y = In (Mul x y)

let_ n exp sub = In (Let n exp sub)

var x = In (Var x)


instance Functor ExprF where
    fmap _ (I x) = I x
    fmap f (Add x y) = Add (f x) (f y)
    fmap f (Mul x y) = Mul (f x) (f y)
    fmap f (Let name x y) = Let name (f x) (f y)
    fmap _ (Var name) = Var name


cata :: Functor f => (f b -> b) -> (Fix f -> b)
cata alg = alg . fmap (cata alg) . out

type Env = Map String Int


exprAlg :: ExprF (Reader Env Int) 
        -> Reader Env Int
exprAlg (I x) = return x
exprAlg (Add x y) = liftA2 (+) x y
exprAlg (Mul x y) = liftA2 (*) x y
exprAlg (Var n) = asks $ fromJust . M.lookup n
exprAlg (Let n exp sub) =
    do x <- asks (runReader exp)
       local (M.insert n x) sub

testexpr = let_ "x" (i 5) (mul (i 3) (add (var "x") (i 1)))

main :: IO ()
main = let env = M.empty
           r   = cata exprAlg testexpr
       in  print $ runReader r env
