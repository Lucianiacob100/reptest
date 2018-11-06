---  Basic Calculator 

 --takes a numeric string  ex . "12341 "---> output is the combination of all possibilities made with 
                           -- parenthesis and + and * operations  
 import Data.Char
 
 -- the number of all possible combinations is 2 ^ (the number of digits in string) / 2
 make_expr _ _ []  = [] 
 make_expr _ _ [x] = [[x]] 
 make_expr s1 s2 (x:xs)  =  let comp = (make_expr s1 s2 xs)  
                                  in
                               (fmap ([x,s1] ++) comp ) ++ (fmap ([x,s2] ++) comp )


-- expressions ls = fmap str_to_expr ls 
 data Aop  = Add | Mult deriving Show

 data Aexp  = Num Int | Exp  Aexp Aop Aexp | Void deriving Show

 
--spliting the string into two pieces 
 split_where :: Char -> [Char] -> ([Char] , [Char])
 split_where ch str = let rs =  str
                          x = takeWhile (/= ch) str  -- ch == '+'
                          y =  tail $ dropWhile (/= ch) str
                           in (x,y)

 s_to_i  s = read  s :: Int

----------------------------------------------------------------------------------------------------- 

 addition [] = False
 addition (x:xs) | x == '+' = True
                 | otherwise  = addition xs

 multiplication [] = False
 multiplication (x:xs) | x == '*' = True
                 | otherwise  = multiplication xs 

 first_part str = if addition str 
                  then fst $ split_where '+' str
                  else if multiplication  str
                  then fst $ split_where '*' str
                  else error "some error.."

 second_part str = if addition str 
                   then snd $ split_where '+' str
                   else if multiplication  str
                   then snd $ split_where '*' str
                   else error "some error.."

 single_number str  =  and [ isDigit  c |    c <- str  ]

 single_operation str  = let n  = length [ c  | c <- str , c == '+' , c == '*' ]
                          in n == 1


--parse the string to an expression tree
 str_to_expr :: String -> Aexp
 str_to_expr str | single_number str = Num $ s_to_i str
                 | single_operation str = if addition str 
                                          then Exp (Num $ s_to_i $ first_part str) Add (Num $ s_to_i $ second_part str)
                                          else if multiplication str  
                                          then  Exp (Num $ s_to_i $ first_part str) Mult (Num $ s_to_i $ second_part str)              
                                          else error "Invalid operation!"
                 | otherwise  = 
                    let c = if '+' `elem` str then '+' else '*'
                        op = if c == '+' then Add else Mult
                          in
                   let  s_str = split_where c str
                        fp  = fst s_str
                        r = snd s_str
            in
         (Exp (str_to_expr fp) op (str_to_expr r)) 
 
 -------------------------------------------------------------------------------------------------------
 apply op = case op of 
           Add -> (+)
           Mult -> (*)

 --evaluating the expression tree to an integer
 eval:: Aexp -> Int
 eval (Num n) = n
 eval (Exp m op n) = apply op (eval m) (eval n)

 eval_string str = eval . str_to_expr $ str                


 fn str n = let list_expr = make_expr '+' '*' str
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
---------------------------------------------------------------


 get_all_ind expr = fst $ 
   foldl (\ac h -> let f_mem = if h == '(' || h == ')' then 
                               (fst ac) ++ [snd ac]
                               else (fst ac)
                       s_mem  = (snd ac) + 1
                  in (f_mem, s_mem)  ) ([] , 0)  expr      
                   
             
 group_by_two [] = []
 group_by_two (x:y:xs)  = (x,y) : (group_by_two xs)

                  
 split_expr e = fst $
                let lsi = group_by_two $ get_all_ind e
                    in_range i lst = or $ fmap (\t -> i >= (fst t) && i <= (snd t ))  lst
                    in 
                foldl (\ac h -> let  f_mem  = if (snd ac) `in_range` lsi then 
                                            (fst ac) ++ ""
                                             else (fst ac) ++  [h]  
                                     s_mem  = (snd ac) + 1
                                 in (f_mem , s_mem))  (  [] , 0 ) e  
 
 intr_first sn e = sn ++ e

 intr_where sn [] = sn
 intr_where sn [x,y] | y == '+' || y == '*' = [x,y] ++ sn
                     |otherwise  = [x,y]
 intr_where sn (x:xs) = if (x == '+' || x == '*') && (not $ isDigit (head xs)) 
                        then  x  : (sn ++ xs)
                        else x : (intr_where sn xs)

 intr_in_place sn "" = sn
 intr_in_place sn (x:xs) | (not $ isDigit x)  = intr_first sn (x:xs)
       {- -}             | otherwise  = intr_where sn (x:xs)

 group lr exp  = snd $
                 until (\t  -> null (fst t)) 
                       (\t -> ((tail (fst t)) , (intr_in_place (head (fst t)) (snd t))
                               ))(lr, exp)

 eval_full_expr ex  = let ex' = split_expr ex
                          lr  = group_by_two $ get_all_ind ex
                          rez  = fmap (\r -> uncurry slice  r ex ) . fmap (\(x,y) -> (x+1,y-1)) $ lr
                          rez' = fmap show $ fmap eval_string rez
                   in  eval_string (group rez' ex')


 eval_all  str = let m = make_expr '+' '*' str
                     le = fmap make_gen_parant m
                     run [] = putStrLn "-------------"
                     run (x:xs)  = x >> (run xs)
                in 
                  run  $ fmap run $
                   fmap (fmap  (\e -> putStr (e ++ " =  ") >>
                        putStrLn (show $ eval_full_expr e))) le
                      

