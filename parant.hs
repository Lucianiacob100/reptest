
 --ads parenthesis to a string of expresion  -- "1+2+3+4"
  -- the number of all possible combinations is 
  -- 2  ^ (the number of numbers in the expression) / 2 - 1

 slice x y ls = [ ls !! i  |   i <- [0..length ls - 1] , i >= x && i <= y ]

 add_paran (i1,i2) str = let fp = take i1 str
                             mid = '(': (slice i1 i2 str) ++ ")"
                             sp  = drop (i2+1) str
                         in fp ++ mid ++ sp

 spl expr =     let t1 =  until (\tp -> (head $ fst tp) == '(')
                                (\tp -> (tail $ fst tp , (snd tp) ++ [(head $ fst tp)]  ))
                            (expr, "" )
                    fsp  = snd t1
                    t2  = fmap tail $
                           until (\tp -> (head $ fst tp) == ')')
                                 (\tp -> (tail $ fst tp , (snd tp) ++ [(head $ fst tp)]  ))
                                 (fst t1 , "")
                    snp =  snd t2
                    trp =  tail $ fst t2
               in  (fsp , (snp , trp))              

 rem_ i  ls = (take i ls) ++ drop (i+1) ls


 make_set lst = foldr (\e rl -> e : (filter (/= e) rl)) [] lst

 make_gen_parant strop |  (length strop) == 3 = [add_paran (0,2) strop]
                       |  (length strop) < 3 = [strop]
                       | otherwise  =
                         let m = length strop - 1
                             pair_of_ind = [(a,b) |  a <- [0..m-2]  , b <- [0..m]  , b-a >= 2 , even a , even b ]    
                             paran_str  = rem_ (((length strop) `div` 2) - 1) $ 
                                        fmap ( flip  add_paran strop)  pair_of_ind
                             reparant tup_expr = let a  = fst tup_expr
                                                     m = fmap (a ++) ( make_gen_parant (fst $ snd tup_expr)) -- [expr]
                                                     m' = fmap (++) m
                                                     b = snd $ snd tup_expr
                                                     op = if null b then "" else [head b]
                                                     rest = if null b then [""] else make_gen_parant $ tail b
                                                     rest' = fmap (op ++) rest         
                                                    in m' <*> rest'       
       in make_set $  (add_paran (0,length strop - 1) strop) : concat ( fmap reparant  $
                                                                  (fmap spl $ 
                                                                     paran_str ))






