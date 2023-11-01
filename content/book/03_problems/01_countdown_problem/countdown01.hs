import Data.List (permutations)
import Foreign.C.Error (resetErrno)

data Op = Add | Sub | Mul | Div 
data Expr = Val Int | App Op Expr Expr 

type Result = (Expr, Int)

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"

instance Show Expr where
   show (Val n)     = show n
   show (App o l r) = brak l ++ show o ++ brak r
                      where
                         brak (Val n) = show n
                         brak e       = "(" ++ show e ++ ")"


apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y 
apply Div x y = x `div` y 

valid :: Op -> Int -> Int -> Bool 
valid Add x y = (x <= y)
valid Mul x y = (x <= y) && (x /= 1) && (y /= 1) 
valid Sub x y = (x > y)
valid Div x y = ((mod x y) == 0) && (y /= 1)

{-
Return a list, so we can return 0 values if there are no solutions without
    having to deal with Maybes
-}
eval :: Expr -> [Int]
eval (Val n) = [n|n>0]
eval (App op left right) = [apply op x y | x <- eval left, y <- eval right, valid op x y]

choices :: [a] -> [[a]]
choices = permutations

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r ) = values l ++ values r 

solution :: Expr -> [Int] -> Int -> Bool 
solution exp nums target = elem (values exp) (choices nums) && eval exp == [target]


split :: [a] -> [([a], [a])]
split xs = [(take n xs, drop n xs) | n <- [1..((length xs)-1)]]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine left right = [App op left right | op <- [Add, Sub, Mul, Div]]

solutions :: [Int] -> Int -> [Expr]
solutions nums target = [e | ns <- choices nums, e <- exprs ns, eval e == [target]]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n>0]
results ns = [res |  (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]
  where combine' (l,x) (r,y) = [(App op l r, apply op x y) | op <- [Add, Sub,Mul, Div], valid op x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', n == m]
