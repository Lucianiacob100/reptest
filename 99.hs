

  -- exercises from https://wiki.haskell.org/99_questions
   --an other related stuff
 import Control.Applicative
 import System.Random
 import Data.Bifunctor
 import Control.Monad

--ex5 -- different ways to reverse a list 
 reversing [] = []
 reversing [x] = [x]
 reversing ls = [(last ls)] ++ (reversing $ drop 1 $ init ls) ++ [(head ls)]


 reversing' ls = foldl (\e h  -> h : e ) [] ls
  -- derived formulas--  reverse  = (foldl  $ flip (:)) []  
                     --  g = foldl $ flip (:) 
                     --  h f xs = f [] xs 
                     --  reverse = h g

 rev [x] = [x] 
 rev ls  = (foldl $ flip (:)) (rev fh) sh
                where
                 m = (length ls) `div` 2
                 fh = take m ls
                 sh = drop m ls

 {-  -}
                     
 reversing'' [x]  = [x]
 reversing'' ls = (reversing'' $ sh ls) ++ (reversing'' $ fh ls)
             where
                 m = (length ls) `div` 2
                 fh = take m 
                 sh = drop m 
  -- derived from property -- reverse (xs ++ ys) = (reverse ys) ++ (reverse xs) 

--ex 6

 palindrom [] = True
 palindrom str = let   l = (length str) - 1
                       m = div l 2
                        in
                    and [ (str !! i) == (str !! (l - i)) |  i <- [0..m] ]
     


--ex 7

 data NestedList a = Elem a | List [NestedList a]
                    deriving (Show,Eq)
 mylist :: NestedList Int
 mylist = (List [Elem 4 , Elem 5 , (List [ Elem 7 ]) , Elem 11 ])

 flatten :: NestedList Int -> [Int]
 flatten (Elem a) = [a]
 flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))
 flatten (List []) = []
------------------------------------------------------------------------------------------------------
--ex 8
 first_ind :: String -> Int
 first_ind  str = until (\i -> if i == (length str) - 1 then True
                               else (str !! i) /= str !! (1 + i)) (1+) 0

 first_part :: String -> Int -> String
 first_part str ind = [str !! c | c <- [0..ind]]

 rest_of_string :: String -> Int -> Int -> String
 rest_of_string str sti endi = [ str !! c |  c <- [sti..endi]]
 

 compress :: [Char] -> [Char]
 compress [] = []
 compress [x] = [x]
 compress str  = cmpf ++ (compress $ rest)
                        where
                           cmpf  = [head fp]
                           fi = first_ind str
                           fp = first_part str fi
                           rest = rest_of_string str (fi + 1) ((length str) - 1)
--------------------------------------------------------------------------------------------------------
--ex 9
  
 pack :: [Char] -> [[Char]]
 pack [] = []
 pack str = [fp] ++ (pack $ rest)
           where
             fi = first_ind str
             fp = first_part str fi
             rest = rest_of_string str (fi + 1) ((length str) - 1)

 
-------------------------------------------------------------------------------------------
 --ex 10
 run_length :: String -> [(Int,Char)] 
 run_length str = let packs = pack str
                     in map (\st -> (length st , head st)) packs 



-------------------------------------------------------------------------------------------
-- ex 11
 data Rl  = Multiple Int Char | Single Char
                             deriving Show
 run_enc ::  String ->  [Rl]
 run_enc str  = let rl = run_length str
                in
             map (\tp -> if (fst tp) == 1 then Single $ snd tp
                         else Multiple (fst tp) (snd tp)) rl
--------------------------------------------------------------------------------                 
-- ex 12

 make_string :: Rl -> String
 make_string (Single c) = [c]
 make_string (Multiple n c) = [ c | _ <- [1..n]]

 decode :: [Rl] -> String
 decode [] = []
 decode (x:xs) = (make_string x) ++ (decode xs) 
---------------------------------------------------------------------------------
 
--ex 13
 encode_mod :: String -> [Rl]
 encode_mod [] =  [] 
 encode_mod str = transf : (encode_mod rest)
               where
             transf = if fi == 1 then Single $ head fp
                      else Multiple (length fp) (head fp) 
             fi = first_ind str
             fp = first_part str fi
             rest = rest_of_string str (fi + 1) ((length str) - 1)


--ex 15
 duplicates [] _ = []
 duplicates str n = [ c | c <- str , _ <- [1..n]]

-------------------------------------------------------------------
--ex 16
 dropevery n [] = []
 dropevery n ls = let l = (length ls)
                      m = if n > l then l else n  
                   in
               [ ls !! i |  i <- [0..(m-2)] ] ++ (dropevery n (drop (m) ls))

--ex 17
 split ::  [a] -> Int ->  Maybe ([a],[a])
 split ls n  = let start = ([] ,ls)
                   loop (a,b) x = if x == 0 then (a,b)
                                  else loop (a ++ [head b] , tail b) (x - 1)
               in  
             if n > ((length ls) - 1) then Nothing
             else Just $  loop start n  
     
--ex 18
 slice :: Int -> Int -> [a] -> [a]
 slice x y ls = [ ls !! i |  i <- [x..y]  ]


--ex 19 
 rotate :: [a] -> Int -> [a]
 rotate ls n | n >= 0 =  (drop n ls) ++ (take n ls )
             |otherwise  = let l = (+ (-1)) $ length ls   in
                      [ls !! i | i <- [l+n+1..l]] ++ [ ls !! i | i <- [0..l+n]]

----ex 20
 rem_at n ls = (ls !! n  ,  (take (n-1) ls) ++ (drop (n+1) ls))

----RANDOM NUMBER GEN
 randnr x  = getStdRandom $ curry  randomR 0 x 
--

--ex 23
 select n ls = let  l = (length ls) - 1
                    lsr = [randnr l | n <- [1..n] ]
                    lsi =   sequence lsr
                         in fmap (\list -> list >>= \e -> return $ ls !! e)
                                 lsi

 



--ex 25
 interleave x ls = [(take i ls) ++ [x] ++ (drop i ls) | i <- [0..length ls]]

 permutations [] = [[]]
 permutations (x:xs) = mconcat $ map (interleave x) $ permutations xs

 raperm  ls  = let prm = permutations ls
                   len  = length prm
                 in
                    do i <- randnr $ len - 1
                       return $ prm !! i  

--ex 26
 --number of combinations
 combi  _ 0 = 1
 combi x y | x == y  = 1
 combi n k = (combi (n-1) (k-1)) + (combi (n - 1) k)

--all subseqquences of a list
 all_subsequences [] = [[]]
 all_subsequences (x:xs) = let alls =  (all_subsequences xs) 
                        in  alls ++ (fmap (x:) alls )

--calculates combinatins by filtering
 combinations ls n = [sl | sl <- all_subsequences ls , length sl == n]

 combinations2 (x:xs) n = let all_sub = (all_subsequences xs)
                              filt = (filter (\sl -> length sl == n))
                                  in 
                            (filt all_sub ++) $ filt $ fmap (x:) all_sub 

 check ls  k  =  (length $  combinations ls k)  == (combi (length ls) k)


--ex 27 -- solved by backtracking 
 create_sublst ls a b c = let alls = all_subsequences ls
                                in 
                          [[sl | sl <- alls , length sl == a],
                           [sl | sl <- alls , length sl == b],
                           [sl | sl <- alls , length sl == c]]


--check if two sets are disjoint
 are_disjoint s1 s2  = and [ not $ x `elem` s2 | x <- s1 ]


--check if a list of sets are all disjoints to one another
 disjointness l  = let starti = 0
                       endi = (length l) - 1
                            in
       and $ map (\tup -> let c  = fst tup
                              ls = snd tup
                           in foldl (\e a -> (are_disjoint a c) && e ) True ls )
                [  ((l !! i ),   (take i l) ++ (drop (i+1) l) )  |   i <- [starti..endi]]
                        


 pick lss ac i  = 
                     if i >= (length $ lss !! (length ac)) then []
                     else ac ++ [ (lss !! (length ac) !! i)]  

   
 choose p lss lt_i ac  r_i =   
                           if length ac == length lss then ac else
                             let possible_next  = pick lss ac r_i in
                               if null possible_next then
                                    if null ac then [] else
                                       let a =  init ac
                                           c = (1+) $ last lt_i
                                           new_lt_i = (init lt_i) ++ [c] 
                                            in
                                     choose p lss new_lt_i a c --backtracks
                               else
                                 if p possible_next then 
                                   choose p lss (lt_i ++ [r_i]) possible_next 0 --continue
                                 else 
                                   choose p lss lt_i ac (r_i+1) --picks next value         
                    
 find_solution lss p =  choose p lss [] [] 0 

 rot_list  ls i =  let y  = (length ls) - 1
                      in
                    if y == i then [] 
                    else (rotate ls i ) : (rot_list ls (i+1))
                     

 solutions  lss p = let h = head lss
                        t = tail lss
                      --  ph = permutations h
                        ph = rot_list h 0
                        make_l = [(ph !! i ) : t |  i <- [0..(length ph)-1]]
                      in
                    map (\l -> find_solution l p) make_l 
 

 make_set lst = foldr (\e rl -> e : (filter (/= e) rl)) [] lst

 solutions_set lss p  = make_set $ solutions lss p
 

 m = ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]

--- 28 insertion sort
 

 insert x xs p = (takeWhile (p x) xs) ++ [x] ++ (dropWhile (p x) xs)

 isort p [x]  = [x] 
 isort p (x:xs) = insert x (isort p  xs) p
 


-- b) sorting lists by the frequency of the lengths of the words
 
 lol = ["abc", "de", "fgh","de", "ijkl", "o","abcdb","bcderf","qe", "mn"]
                           
 index_table :: (Eq a, Num a1) => [a] -> Int -> (a, [a1])
 index_table ls ind = let 
                       h  = ls !! ind
               in
      first (\_ -> h) $
          foldl (\e a -> if a == h then ((1+) $ fst e , (snd e) ++ [fst e]) 
                         else ((1+) $ fst e , snd e) ) (0 , []) ls

 all_indeces lsz ind  = let max_size  = (length lsz) 
                           in 
                          if ind == max_size then [] 
                          else (index_table lsz ind :) $ all_indeces lsz (ind + 1) 

 sorted_sz lsz = isort (\a b -> (fst a) > (fst b)) lsz


 retrieve []   _   =     []
 retrieve srted_siz_ls initial_list =  (make_list (head srted_siz_ls) initial_list) :
                                       (retrieve (tail srted_siz_ls) initial_list)
          where
             make_list [] _  = [ ]
             make_list (x:xs) init_ls = (init_ls !! x) : (make_list xs init_ls)


 sort_ls_by_freq ls  = let leng = map length ls
                           sri  = sorted_sz   $
                                    [   (length x , x)       |  (_,x) <-  all_indeces leng 0]
                           filtered_ind = [ x |     (_,x) <- sri   ] 
                     in
        make_set $   concat $ retrieve filtered_ind ls


 --Fermat test for prime numbers
 square x = x ^ 2
 
 expmod b e m = if e == 1 then b 
                else if even e then
                     mod (square ( expmod b (div e 2) m)) m
                     else mod (b * (expmod b (e - 1) m)) m
                    
 fermat_test n =  fmap (2+) ( randnr (n - 3)) >>= \r -> return ( (expmod r n n) == r)
 
 is_prime n  times =  fmap and $ sequence [ fermat_test n | _ <- [1..times]] 
 
--ex 32 greatest common divisor
 mtoInt ::  String ->  Int
 mtoInt = \x -> (read x :: Int)
 
 get_Int :: String -> IO Int
 get_Int msg  = ((\m -> putStrLn m >> getLine) >=> (\s -> return $ mtoInt s)) msg 


--Euclid's algoritm
 gcd' a b  = let m  = mod a b
              in
            if m == 0 then b 
            else gcd' b m
 
 gcd_m  = get_Int "First number:"  >>=
         \fsn -> get_Int "Second number:" >>=
         \scn ->   return $ ("The greatest common divisor is" ++) $ 
                              show $ gcd' fsn scn  
                 
--Euclidian extended algorithm
  -- m*a + n * b   = gcd a b
   -- input: a,b output: [gcd a b , m , n]
 ext_gcd :: Integral a => a -> a -> [a]
 ext_gcd a b = let [mpp,mp,npp,np,aa,bb] = [1,0,0,1,a,b]
                   myargs   =  [mpp,mp,npp,np,aa,bb]
                   trans args = if (args !! 5) == 0 then 
                                    let rez = [args !! 4 , args !! 0 , args !! 2]
                                       in
                                     if (args !! 4) >= 0 then rez
                                     else fmap (*(-1)) rez
                                else 
                                  let q = div (args !! 4)  (args !! 5)
                                      r = rem (args !! 4) (args !! 5)
                                      aa' = args !! 5
                                      bb' = r
                                      m = (args !! 0) - q * (args !! 1)
                                      n = (args !! 2) - q * (args !! 3)
                                      mpp' = (args !! 1)
                                      mp' = m
                                      npp' = args !! 3
                                      np' = n
                               in
                                   trans [mpp',mp',npp',np',aa',bb']
                 in trans myargs
                 
--ex 33
 coprime a b = (gcd' a b) == 1 

-- ex 34
 tot m = length  [  r | r <- [1..m-1] , coprime m r]

--ex 36
 prime_factors x act v = let la = last act
                             tla = [fmap (1+) la]
                             ia = init act
                        in
                   if x == 1 ||  ((fst la) == x) then  (ia ++ tla) else
                         if x `mod` (fst la) == 0 
                         then prime_factors (div x $  fst $ la) (ia ++ tla) v
                         else  prime_factors x  (act ++ [((fst la) + 1 , 0)] ) v
            
 prime_f s  =  let vf = div s 2 in
            filter (\t -> snd t /= 0 ) $  prime_factors s [(2,0)] vf

--ex 37
 toti :: Int -> Int
 toti x = let lsp = prime_f x
            in
          foldl  (\ e tup -> let a  = (fst tup) - 1
                                 b = (fst tup) ^ ((snd tup) - 1)
                           in  e * a * b)
                 1 lsp

-- ex 39
 multiple x y = (x `mod` y) == 0

 sieve [] = []
 sieve range = (head range) : (sieve (filter (\x -> not $ multiple x (head range))  
                                             (tail range)))
 createRP a b = sieve [a..b]

--ex 40

 goldbach nr = if nr == 2 then Just [1,1]
               else if nr < 2 then Nothing
               else   
                Just $ let  rop = [1] ++ sieve [2..nr]
                            tn = combinations rop 2
                     in
                      head . filter (\t -> t /= []) $ 
                       map  (\ln -> if sum ln  == nr then ln else []) tn

--ex 41
  -- prints goldbach sums for even numbers

 goldb_range a b  =  (() <$ ) $  
                     sequence . 
                     fmap putStrLn $ 
                     fmap (\l -> (show $ sum l) ++ " = " ++ 
                                 (show $ l !! 0 ) ++ " + " ++ 
                                 (show $ l !! 1)) $
                     concat 
                         [ fn $ goldbach x |  x <- [a..b] , even x]
                   where
                    fn (Just x) = [x]
                    fn Nothing  = []   


