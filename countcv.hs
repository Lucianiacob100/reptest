    module Countcv where

--    citeste n linii si intoarce o lista
-- cu numarul de vocale si de consoane de pe fiecare linie
 vocale = ['a' , 'e' , 'i' , 'o' , 'u']
 

 lop = [ (\c -> c `elem` vocale), ( not <$> lop !! 0)]
 
 count_l str  p = sum [ 1  |   c <-   str , p c  ]

 prt n = putStrLn $ (++ (show n))  "Introduceti linia cu numarul "
  
 getNchars nc =  if nc == 0 then return []
                 else (fmap (:) getChar) <*> (getNchars (nc - 1))   

 llns nl nc = [ getNchars nc | _ <- [1..nl]]
 

 lf n = [  prt i >> p <* putStrLn "" |  (i,p) <- zip [1..n]  (llns n 4)  ] 

 pc = fmap $ fmap (\s-> (("vocale : " ++) $ show $ count_l s (lop !! 0)) ++ 
                        ((" consoane : " ++) $ show $ count_l s (lop !! 1)))

 run_lf  n =  pc $ sequence $ lf n


