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
    fromEnum m  = let rm = reduce m in
                           case rm of
                          Zero -> 0 
                          (Pred x) -> (-1) +  (fromEnum x)
                          (Succ x) -> 1 + (fromEnum x)
           
    enumFrom m = (m :) $ enumFrom $ succ m
   

 instance Num Nat where
    negate Zero  = Zero
    negate (Succ m) = Pred $ negate m
    negate (Pred m) = Succ $ negate m
                     
    abs Zero = Zero
    abs m  = let rm  = reduce m in
                       case rm  of
                       Zero -> Zero
                       (Pred x) -> negate m
                       _        -> m

   
    m + Zero = m
    Zero + m = m
    (Succ m) + (Succ n) =  Succ ((Succ m) + n)
    (Succ  m) + (Pred n) = (Succ m) + n
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
                sign :: Nat -> Char
                sign nr = case reduce nr of
                         (Pred _) -> '-'
                         _        -> '+'
                mult :: Nat -> Nat -> Nat
                mult m Zero = Zero
                mult m (Succ n) = (m `mult` n) + m
            in case sign m of
               '+' -> case sign n of
                      '+' -> m `mult` n
                      '-' -> negate $ (mult m) $ abs n
               '-' -> case sign n of
                      '+' -> negate $ mult (abs m) n
                      '-' -> mult (abs m) (abs n)

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

 

      
     
 
 


       

