data LE
    = Var String
    | App LE LE
    | Abs String LE 
    deriving (Eq,Show)
    
remove _ [] = []
remove x (y:ys) = 
    if x == y then ys else y : remove x ys
    
union [] ys = ys 
union (x:xs) ys = 
    if elem x ys then union xs ys else x : union xs ys
    
fv (Var v) = [v]
fv (App e1 e2) =
    let 
        f1 = fv e1
        f2 = fv e2
    in
        union f1 f2
fv (Abs v e) =
    remove v (fv e)


----------------------------------------------
-- subst what var where
subst w var ins = run ins
    where 
        fvs = fv w
        comb (Just e1) (Just e2) = Just $ App e1 e2 
        comb _ _ = Nothing
        run vv@(Var v) = if v == var then Just w else Just vv
        run vv@(Abs v e) = 
            if elem var (fv e) && elem v fvs then Nothing else         
            if v == var then Just vv else 
            case run e of 
                Nothing -> Nothing
                Just e -> Just $ Abs v e
        run (App e1 e2) = comb re1 re2
            where
                re1 = run e1 
                re2 = run e2

----------------------------------------------
-- varianta bez bonusu
pp' x = ppp x >> putStrLn ""

ppp (Var v) = putStr v 
ppp (App e1 e2) = do
    putStr "("
    ppp e1 
    putStr " "
    ppp e2
    putStr ")"
ppp (Abs v e) = do
    putStr "(\\ "
    putStr v
    putStr " -> "
    ppp e 
    putStr")"