---  Basic Calculator 
   --mathematical expression evaluator
     
 import Data.Char
 
 -- the number of all possible combinations is (the number of operators) ^ ((the number of digits in string) - 1)
  --lopr is the list of operators
 make_expr  _ []  = [] 
 make_expr  _ [x] = [[x]] 
 make_expr lopr (x:xs)  =  let comp = (make_expr lopr xs)  
                               fsp  = fmap (\op -> [x,op]) lopr  {- ----> ["1+" , "1*" , "1-" ....] -}                              
                               fsp'  = fmap (\m -> fmap (m ++)) fsp  
                             in
                         concat $ fsp' <*> [comp]
 
-- expressions ls = fmap str_to_expr ls 
 data Aop  = Add | Mult | Sub  | Div deriving Show

 data Aexp  = Num Float | Exp  Aexp Aop Aexp | Void deriving Show

 s_to_i  s = read  s :: Float

----------------------------------------------------------------------------------------------------- 


 single_number str  =  and [ isDigit  c |    c <- str  ]
                     

--cascading functions for finding the right operator and index
 find_as pc cc (x:xs) exp | length (x:xs) == 1 = find_md 0 0 exp exp
                          | x `elem` "()" =  find_as (pc + 1) (cc + 1) xs exp
                          | (x `elem` "+-") && (even pc) = (x,cc)
                          | otherwise    =  find_as pc (cc + 1) xs exp

 find_md pc cc (x:xs) exp | length (x:xs) == 1 = find_first_as  0 exp exp
                          | x `elem` "()" =  find_md (pc + 1) (cc + 1) xs exp
                          | (x `elem` "*/") && (even pc) = (x,cc)
                          | otherwise    =  find_md pc (cc + 1) xs exp

 find_first_as cc (x:xs) exp | length (x:xs) == 1 = find_first_md  0 exp
                             | x `elem` "+-"  = (x,cc)
                             | otherwise  = find_first_as (cc + 1) xs exp

 find_first_md cc (x:xs) | length (x:xs) == 1 = (' ' , -1) --search failed
                         | x `elem` "*/"  = (x,cc)
                         | otherwise  = find_first_md (cc + 1) xs
 
 deparant (x:xs) | x == '(' = deparant xs
                 | last (x:xs) == ')' = init (x:xs)
                 | otherwise = x:xs

 numarize  = Num  . s_to_i 
 str_to_expr' str | single_number str = numarize str
                  | otherwise         =  let pos_op  = find_as 0 0 str str 
                                             op_s = fst pos_op
                                             index = snd pos_op
                                             first_part = take index str
                                             second_part = drop (index + 1) str
                                             form_expression p1 p2 op_d = Exp (str_to_expr' . deparant $  p1) op_d 
                                                                              (str_to_expr' . deparant $ p2)
                                            in
                                         case op_s of
                                         '+' -> form_expression first_part second_part Add
                                         '-' -> form_expression first_part second_part Sub
                                         '*' -> form_expression first_part second_part Mult
                                         '/' -> form_expression first_part second_part Div
                                         _   -> error "generic error"
 -------------------------------------------------------------------------------------------------------
 apply op = case op of 
           Add -> (+)
           Mult -> (*)
           Sub  -> (-)
           Div  -> (/)
  
 --evaluating the expression tree to an integer
 eval:: Aexp -> Float
 eval (Num n) = n
 eval (Exp m op n) = apply op (eval m) (eval n)

 eval_string str = eval . str_to_expr' $ str    --modified here --needs testing            


 fn str n = let list_expr = make_expr ['+' ,'*'] str
                  in 
              filter (\s -> eval_string s  == n) list_expr



 --ads parenthesis to an expression  -- "1+2+3+4"
  -- the number of all possible combinations is 
  -- 2  ^ (the number of terms in the expression) / 2 - 1

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


 make_set  = foldr (\e rl -> e : (filter (/= e) rl)) [] 

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

 eval_all lopr  str =
                 let m = make_expr lopr str
                     le = fmap make_gen_parant m
                     run [] = putStrLn "-------------"
                     run (x:xs)  = x >> (run xs)
                in 
                  run  $ fmap run $
                   fmap (fmap  (\e -> putStr (e ++ " =  ") >>
                        putStrLn (show $ eval_string e))) le
                      
