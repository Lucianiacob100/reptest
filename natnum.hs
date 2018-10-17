 module Natural where


 data Nat = Succ Nat | Zero | Pred Nat
            deriving Show


 reduce :: Nat -> Nat
 reduce x  = case x of
             Zero     -> Zero
             (Succ (Succ m)) -> (Succ (Succ $ reduce m))
             (Pred (Pred m)) -> (Pred (Pred $ reduce m))
             (Pred (Succ m)) -> reduce  m
             (Succ (Pred m)) -> reduce m
             _               -> x      

 instance Enum Nat where
    succ m = case m of
             Zero -> Succ Zero
             (Succ x ) -> (Succ m)
             (Pred x)  -> x

    pred m  = case m of
              Zero -> Pred Zero
              (Succ x) -> x
              (Pred x) -> Pred $ Pred x
  
    toEnum x | x == 0 =  Zero
             | x < 0  = Pred $ toEnum $ x + 1
             | otherwise = Succ $ toEnum $ x - 1

    fromEnum Zero = 0 --in case of first evaluation is zero
    fromEnum m  = 
                     case reduce m of
                          Zero -> 0 
                          (Pred x) -> (-1) +  (fromEnum x)
                          (Succ x) -> 1 + (fromEnum x)
           
    enumFrom m = (m :) $ enumFrom $ succ m

 instance Bounded Nat where
    minBound = toEnum (-100)
    maxBound  = toEnum 100
   

 instance Num Nat where
    fromInteger m  =  let x  = fromIntegral m in
                          toEnum x  

    signum Zero = 1
    signum m = case reduce m of
                    (Pred _) -> (-1)
                    _ -> 1
    
    negate Zero  = Zero
    negate (Succ m) = Pred $ negate m
    negate (Pred m) = Succ $ negate m
                     
    abs Zero = Zero
    abs m  = case reduce m of
             Zero -> Zero
             (Pred x) -> negate m
             _        -> m

   
    m + Zero = m
    Zero + m = m
    (Succ m) + (Succ n) =  Succ ((Succ m) + n)
    (Succ  m) + (Pred n) = Pred $ (Succ m) + n
    (Pred m) + (Succ n) = (Succ n) + (Pred m)
    (Pred m) + (Pred n) =  (pred m) + (pred n)  

    m - Zero = m
    Zero - m = negate m
    (Succ m) - (Succ n) =  m - n
    (Succ m) - (Pred n) = Succ $ (Succ m) - n   
    (Pred m) - (Succ n) = Pred $  m - (Succ n)  
    (Pred m) - (Pred n) = m  - n
 

 
    m * Zero = Zero
    Zero * m  = Zero
    m * n = let 
                mult :: Nat -> Nat -> Nat
                mult m Zero = Zero
                mult Zero m = Zero
                mult m (Succ n) = (m `mult` n) + m
            in case signum m of
               (Pred _ ) -> case signum n of
                            (Succ _) -> negate $ (abs m) `mult` (abs n)
                            (Pred _) -> (abs m) `mult`  (abs n)
               (Succ _ ) -> case signum n of
                      (Succ _) -> (abs m) `mult` (abs n)
                      (Pred _) -> negate $ (abs m) `mult` (abs n)
             




 instance Eq Nat where
      Zero == Zero  = True 
      _  == Zero = False
      Zero == _  = False
      m == n  = let a  = reduce m
                    b = reduce n 
                  in  case a of
                      (Succ x) -> case b of
                                  (Succ y) -> x == y
                                  _ -> False
                      (Pred x) -> case b of
                                  (Succ _) -> False
                                  (Pred y) -> x == y 

      m /= n = not $ m == n

 
 instance Ord Nat where
    a > b  = case reduce a of
             Zero -> case reduce b of
                     Zero -> False
                     (Succ m) -> False
                     _        -> True
             (Succ m) -> case reduce b of
                         Zero -> True
                         (Succ n) -> m > n
                         (Pred n) -> True    
             (Pred m)  -> case reduce b of
                          Zero -> False
                          (Succ _) -> False
                          (Pred n) ->  m > n

      
    a < b = (not (a > b)) && (a /= b)
 
    a >= b = (a > b) || (a == b)
   
    a <= b  = (a < b) || (a == b)

    max a b = if a > b then a else b
 
    min a b  = if a < b then a else b
       
    compare a b = if a > b then GT else LT

 
