module BooleanExpr where

data Expr a
  = Value Int
  | And (Expr a) (Expr a)
  | Xor (Expr a) (Expr a)
  deriving(Show, Eq)

eval :: Expr a -> Int
eval (Value e) = e
eval (And (Value 0) e) = eval e
eval (And e (Value 0)) = eval e
eval (And e1 e2) = eval e1 + eval e2
eval (Xor (Value 0) _) = 0
eval (Xor _ (Value 0)) = 0
eval (Xor e1 e2) =
  if eval e1 == 0
    then 0
    else eval e1 * eval e2
    
    
data BE 
  = BTrue 
  | BFalse 
  | BAnd BE BE 
  | BOr BE BE 
  deriving (Show, Eq)

eVal BTrue = True
eVal BFalse = False
eVal (BAnd BTrue e) = eVal e
eVal (BAnd e BTrue) = eVal e
eVal (BAnd BFalse _) = False
eVal (BAnd _ BFalse) = False
eVal (BOr BTrue _) = True
eVal (BOr _ BTrue) = True
eVal (BOr BFalse e) = eVal e
eVal (BOr e BFalse) = eVal e
    
main = do
    putStr "eval (Val 4) >>>> "
    print (eval (Value 4))
    putStr "eval (Add (Val 4) (Val 5)) >>>> "
    print (eval (And (Value 4) (Value 5)))
    putStr "eval (Mul (Val 0) (Val 9)) >>>> "
    print (eval (Xor (Value 0) (Value 9)))
    putStr "\n"