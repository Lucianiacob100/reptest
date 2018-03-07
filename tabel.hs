 
 import Data.Monoid
 import  Data.Char
 import Control.Applicative 
 import Data.Maybe

 {- a table as a linked list of Rows
   ___        ___                        ___________
  | * | ---> | * | --->           * ---> EndPointr  |
  |_|_|      |___|         . . . .  .    ___________|
    |
    |____(                             )
         ( (key, name, job) )

  -}
 
 data Job = Brutar String 
               |Mecanic String 
               |PilotF1 String 
               |Electrician String 
               |Medic String
               |Nimic            deriving (Show,Eq)

 data Table a b  c = EndPointr | Row (a,b,c) (Table a b c) 
                            deriving (Show, Eq) 

 type Key = Int
 type Name = String
 type Triple = (Key,Name,Job)
 type HsType = Table Key Name Job --declaring type synonyms 

 mytable :: HsType
 mytable =        Row (1, "Lucian",  Brutar "de cativa ani")
                ( Row (1, "Lucian",  Brutar "de cativa ani") 
                ( Row (2, "Ionut",   PilotF1 "in timpul liber")
                ( Row (3 ,"Marian",  Mecanic "categoria 1") 
                ( Row (2, "Ionut",   PilotF1 "in timpul liber")
                ( Row (4, "Cristian",Electrician "priceput")
                ( Row (3 ,"Marian",  Mecanic "categoria 1")
                ( Row (1, "Lucian",  Brutar "de cativa ani") 
                ( Row (5, "Mihai" ,  Medic "oftalmolog")( EndPointr)
                                           ))))))))


 instance (Eq a , Eq b , Eq c) => Monoid (Table a b c) where
    
     mempty  = EndPointr
     
     mappend = tablejoin

     mconcat lst = foldr tablejoin mempty  lst
     --mconcat lst = foldr1 tablejoin   lst
     --mconcat lst = foldr mappend mempty  lst



   --class Selectors which implements three selectors methods  
  --for any data of kind * -> * -> * -> *    
 class  Selectors s where
     first ::  (Elements a , Elements b , Elements c) =>  s a b c -> a
     sec   ::  (Elements a , Elements b , Elements c) =>  s a b c -> b
     thrd  ::  (Elements a , Elements b , Elements c) =>  s a b c -> c 

 instance  Selectors (,,)  where
     first (a,b,c) = (\(x,y,z) -> a) $ (a,b,c)

     sec (a,b,c)   = (\(x,y,z) -> y) $ (a,b,c)

     thrd (a,b,c)  = (\(x,y,z) -> z) $ (a,b,c)


 --data container for the methods of selector class
 data Selec s a b c e = S {
                  s1 :: s e b c -> e  ,  
                  s2 :: s a e c -> e  ,
                  s3 :: s a b e -> e 
                 }
 t :: (Selectors s ,  Elements e) => Selec s Int String Job e 
 t = S first sec thrd


----------------

 class Elements a where
     triv :: a -> a

 instance Elements Int where
    triv n = n

 instance Elements ([] a) where
    triv [s] = [s]

 instance Elements Job where
     triv _ = Brutar ""

 instance Elements (Table a b c) where
     triv EndPointr = EndPointr
     triv (Row x y) = (Row x y) 
-------------------------------------------
 class  GeneralMet a where 
     restOfList    :: a -> a
     nrOfRows      :: a -> Int
     tablejoin     :: Eq a => a -> a -> a
     removeAtIndex :: Int -> a -> a
     takeFirst     :: Int -> a -> a
     dropFirst     :: Int -> a -> a
     removeElemE   :: Eq a =>  a -> a 
     dropLast      :: Eq a => Int -> a -> a
     slicetable    :: Eq a =>  a -> Int  -> Int -> Maybe a      
   

 instance GeneralMet (Table k n w) where

      restOfList (Row x y) = y
      restOfList EndPointr = EndPointr

      nrOfRows EndPointr = 0
      nrOfRows (Row x y) =  1 + (nrOfRows y) 
    
      --tablejoin EndPointr EndPointr = EndPointr
      tablejoin (Row x y) table | y == EndPointr =  (Row x table) 
                                |otherwise       = Row x (tablejoin y table)

      removeAtIndex nPos EndPointr = EndPointr
      removeAtIndex nPos (Row x y) | nPos == 0 = y
                                   |otherwise = Row x (removeAtIndex (nPos-1) y)   

     
      takeFirst _ EndPointr = EndPointr
      takeFirst ind (Row x y) | ind == 0 = EndPointr
                              |otherwise = (Row x (takeFirst (ind - 1) y)) 


      dropFirst _ EndPointr   = EndPointr
      dropFirst ind table      | ind == 0  = table
      dropFirst ind (Row x y)  | ind == 1 = y
                               |otherwise = dropFirst (ind-1) y  

      removeElemE (Row x y) | y == EndPointr = EndPointr
                            |otherwise = Row x (removeElemE y)


      dropLast _ EndPointr = EndPointr
      dropLast ind table  | ind == 0 = table
                          |otherwise = dropLast (ind - 1) (removeElemE table)
 


      slicetable  (Row x y) ind1 ind2 | ind1 > ind2  = Nothing
      slicetable  (Row x y) ind1 ind2 | ind1 == ind2 = Just (Row x EndPointr)
                                      |otherwise     = Just ((dropFirst ind1) . 
                                                         (dropLast (l - ind2))  $ (Row x y))
                                    where l = nrOfRows (Row x y)

--------------------------------------------------------

 nilTriple :: Triple
 nilTriple = (-1 , "" ,Nimic )

     
 cons :: (a, b, c) -> Table a b c -> Table a b c
 cons trip (Row x y) = Row trip (Row x y)

 addToEnd :: (Eq a, Eq b, Eq c) => (a, b, c) -> Table a b c -> Table a b c
 addToEnd trip EndPointr = Row trip EndPointr
 addToEnd trip (Row x y) | y == EndPointr =  (Row x (Row trip EndPointr))
                         |otherwise = Row x (addToEnd trip y)

 reversetable :: HsType -> HsType
 reversetable (Row x y)| y == EndPointr = (Row x y)
                       |otherwise       = addToEnd x (reversetable y) 


 firstntup :: Int -> HsType -> [Triple]
 firstntup 0 (Row x y) = [x]
 firstntup n (Row x y) = [x] ++ (firstntup (n-1) y)
 firstntup n EndPointr = [nilTriple]

  
 ---------------------------------------
-------------------------------------------------------------------

---------------------------------------------------
 mapovert :: (Triple -> Triple ) -> HsType -> HsType
 mapovert f EndPointr = EndPointr
 mapovert f (Row x y) = Row (f x) (mapovert f y)


 filterovert :: (Triple -> Bool) -> HsType -> HsType
 filterovert p EndPointr = EndPointr
 filterovert p (Row x y) | (p x) =  Row x (filterovert  p y)
                         |otherwise = filterovert p y
----------------------------------------------------------------------

 searchfordup :: Int-> Int -> Triple -> HsType -> (Int, Int)
 searchfordup _ _ _ EndPointr = (0,-1)
 searchfordup startPos itPos trip (Row x y) | trip ==  mytrip = (startPos,itPos)
                                            |otherwise = searchfordup startPos (itPos + 1) trip y
                     where mytrip = fstTrip y 


 duplicateentries  :: Int -> Int -> HsType -> [(Int,Int)]
 duplicateentries  i j (Row x y )| y == EndPointr = [(0,-1)]
 duplicateentries i j (Row x y) = (searchfordup i j x (Row x y)) : (duplicateentries (i+1) (j+1) y)
  
 -- onlyduplicates :: HsType -> [(Int,Int)] 
 --onlyduplicates  table = filter (/= (0,-1) ) (duplicateentries 0 1 table)

 removedup :: HsType -> HsType                             
 removedup   EndPointr = EndPointr  
 removedup (Row x y) = let
    filterbyfirst :: HsType -> HsType
    filterbyfirst EndPointr = EndPointr
    filterbyfirst (Row  x y) = Row x (filterovert (/= x) y)
             in  filterbyfirst  (Row x (removedup  y))



 foldrighttable fn en  EndPointr = en
 foldrighttable fn en (Row x y) =  (fn x (foldrighttable fn en y)) 
 
 applypred op table = Row (fstTrip table) 
                    (foldrighttable (\ a b  ->  if   (op a  (fstTrip table)) then b
                                                 else (Row a b))
                                     EndPointr table)

 
 remdup :: HsType -> HsType
 remdup (Row x EndPointr) = (Row x EndPointr)
 remdup (Row x y) = Row x (applypred (==) (remdup y))


 removeduplicates :: HsType -> HsType
 removeduplicates = applypred (==) . remdup
 {--}


---------------------------------------------------------------

 searchfortrip :: Triple -> HsType -> Int 
 searchfortrip trip EndPointr = 1       
 searchfortrip trip (Row x y) | trip == x = 0
                              | otherwise = 1 + searchfortrip trip y

 tripleexists :: Triple -> HsType -> Bool
 tripleexists trip table | n == l = False
                         |otherwise = True
         where n = searchfortrip trip table
               l = nrOfRows table 



 searchforkey :: Key -> HsType -> Int --returns an index
 searchforkey key (Row x y) |(y == EndPointr) && ((first x) /= key) = 1
                            |key == (first x) = 0
                            |otherwise = 1 + searchforkey key y

 keyexists :: Key -> HsType -> Bool
 keyexists key table | n == l = False
                     |otherwise = True
          where n = searchforkey key table
                l = nrOfRows table

 searchforname :: Name -> HsType -> Int  
 searchforname name (Row x y) | (y == EndPointr) && ((sec x) /= name) = 1
                              | name == (sec x) = 0
                              | otherwise = 1 + searchforname name y 
 
 nameexists :: Name -> HsType -> IO ()
 nameexists name table | n == l = putStrLn "Numele nu exista in tabel"
                       |otherwise = putStrLn ("Numele se afla in tabel la index " ++ (show n))
           where n = searchforname name table
                 l = nrOfRows table
 -------------------------------------------------------------------
---------------------------------------------------------------------

 searchForElem :: (Elements a, Eq a) => a -> (Triple -> a ) -> HsType -> Maybe Triple
 searchForElem e sel EndPointr = Nothing
 searchForElem e sel (Row x y) = (Just e) >>= 
                                 \el -> (Just (sel x)) >>=
                                 \ en -> if el == en then 
                                         return x
                                         else (searchForElem e sel y) 
                                                      

-----------     -----         -----     -----------------------------

 fstTrip :: HsType -> Triple
 fstTrip (Row x y) = x
 fstTrip EndPointr = nilTriple



 getTriPos :: Int -> HsType -> Triple
 getTriPos 0 table = fstTrip table
 getTriPos n (Row x y) = getTriPos (n-1) y
 getTriPos n EndPointr = nilTriple


 getTheE ::  Elements a  => Int -> (Triple -> a ) -> HsType ->  a
 getTheE n selec table = selec . getTriPos n $ table
       
 data Tri a b  c =  Fs a | Sc b | Th c | Err_
                             deriving (Show , Eq)


 getf :: Selectors s => IO (Tri  ((s Key Name Job) -> Key)
                                 ((s Key Name Job) -> Name)
                                 ((s Key Name Job) -> Job))
 getf = getLine >>= \s -> return (case s of
                                       "key" -> Fs (s1 t)
                                       "name" -> Sc (s2 t)
                                       "job"  -> Th (s3 t))

 mytables :: [(String,HsType)]
 mytables = [("mytable",mytable) , ("null", EndPointr)]

 lookfor :: String -> [(String,HsType)] -> Maybe HsType
 lookfor s (t:ts) | ts == []   && (s /= (fst t))  = Nothing
                  | s == (fst t) = Just (snd t)
                  | otherwise    = lookfor s ts


 mgetE ::  IO ()
 mgetE  = putStrLn " DENUMIREA ELEMENTULUI : " >> getf >>= 
                        \f ->  putStrLn "De la pozitia..:" >>
                         getLine >>= 
                        \i -> if not $ verify i then ( (print "introduceti doar caractere numerice") >> mgetE ) 
                               else ( return (mtoInt i) <*
                        (putStrLn "Din tabelul...") >>=
                        \ind -> getLine >>= \tname -> ( let r = (lookfor tname mytables) in 
            case r of
            Nothing -> print "Exit"
            _       -> ( return (fromJust r) <*
                         print "Rezultatul cautarii este ..: " >>= 
                         \tab -> return (getTriPos ind tab ) >>=
                         \trip -> return (case f of
                                             (Fs f)  -> Fs (f trip)
                                             (Sc f) -> Sc  (f trip)
                                             (Th f)  -> Th (f trip))>>=
                         \elem -> print elem >> putStrLn "Doriti sa continuati?" >>
                         ( let fn =    getChar >>= \c -> case c of
                                                         'y' -> putStrLn "" >>  mgetE 
                                                         'n' ->  print "End Of Program"
                                                         _  -> print "optiune invalida!(Aveti de ales y/n)" >> fn
                                                   in 
                                                       fn))) )
                  
              
------------------------------------------------------------------------------
     
 fltFirst :: Elements a =>  Int ->  HsType-> (Triple -> a) -> [a]
 fltFirst n table selec = (firstntup n table) >>= \ e -> [(selec e)]
-----uses the filterall functions

 printElm :: Show a => [a] -> IO ()
 printElm [] = print ""
 printElm (x:xs) = return (x:xs) >>= \s -> print x >> printElm xs


 printRows :: [(Triple)] -> IO()
 printRows [] = putStr "\n"
 printRows (x:xs) = return (x:xs) >>= 
                    \s -> putStr (show ((s1  $ t) x)) >>
                    putStr "\t" >>
                    putStr ((s2 $ t) x) >> 
                    putStr "\t" >>
                    putStr (show ((s3 $ t) x)) >>
                    putStr "\n" >>
                    printRows xs

 printTable :: Int -> HsType -> IO ()
 printTable n table = printRows . firstntup n $ table


 printSelected :: Show a => Int-> HsType -> (Triple -> a) -> IO ()
 printSelected n table selec =  printElm $ ((firstntup n table) >>= \e -> [(selec e)]) 
                            
 printAll :: Show a => (t -> [a]) -> t -> IO ()
 printAll  filterf table =  putStrLn ("Elementele selectate din tabel sunt: \n" <>     
                                  (mconcat  
                                    ((fmap show $ (filterf table)) >>=                       
                                      (\x -> [x <> "\n"]))))                              
--prints all  elements each one on a row

 mtoInt ::  String ->  Int
 mtoInt = \x -> (read x :: Int)

 makeTriple :: Key -> Name -> Job -> Triple
 makeTriple k n m =  (k,n,m)

 findind :: String -> Char -> Int
 findind [x]  c     |  x == c = 0
                    | otherwise = 1  
 findind (x:xs)  c  |   x == c = 0
                    | otherwise = 1 + (findind  xs c )

 split :: String -> Int -> (String,String)
 split s i = ((take i s)   ,  (drop (i+1) s))

 splitString :: String -> Char -> (String, String)
 splitString str c = split str (findind str c)  

 verify :: String -> Bool
 verify str = and [isDigit c |    c <- str   ]

 data Err_handler a  = Ok a | Warning  String
            deriving (Show,Eq)

 warning_1 = Warning " Caracterele introduse nu sunt numerice!"
 warning_2 = Warning " Valoarea introdusa nu este valida !"

 mt :: IO (Err_handler Triple )
 mt = getLine >>= \s1 -> if not $ (verify s1) then return  warning_1 else 
      getLine >>= \s2 -> 
      getLine >>= \s3 -> let ms = fst (splitString s3 ' ')
                             jd = snd (splitString s3 ' ')
                             kk = mtoInt s1
                             in
                         return (if ms `elem` ["Brutar","Mecanic", "PilotF1" , "Electrician" ,"Medic"] then
                                    case ms of
                                     "Brutar"      -> Ok (kk, s2, Brutar jd)
                                     "Mecanic"     -> Ok (kk, s2, Mecanic jd)
                                     "Pilot F1"    -> Ok (kk, s2, PilotF1 jd)
                                     "Electrician" -> Ok (kk, s2, Electrician jd)
                                     "Medic"       -> Ok (kk, s2, Medic jd)                                    
                                     else warning_2 )
    
 --transf :: Err_handler a b -> a
 transf (Ok x) =  x    
 transf _ = nilTriple
 
 insertNew insert_f table = mt >>= \ t -> return ( insert_f (transf t) table)      
--cons  --insertatindex

 
 --given a Job, makes a new IO Triple 
 makeT_j :: Job -> IO Triple
 makeT_j msr = getLine >>= \k -> getLine >>= \n -> return ((mtoInt k) , n , msr) 


 mAddElem :: (Triple -> HsType -> HsType) ->
               Job -> HsType -> IO HsType
 mAddElem addf t_msr table = pure cons <*> 
                            (makeT_j t_msr) <*>
                             pure table
 --cons --addToEnd --insert at beginning or the end

 

 insertAtIndex :: Triple -> Int -> HsType -> HsType
 insertAtIndex trip nPos EndPointr   = EndPointr
 insertAtIndex trip nPos (Row x y)  | nPos == 0 = Row trip (Row x y) 
                                    | nPos == 1 = Row x (Row trip y)
                                    | otherwise = Row x (insertAtIndex trip (nPos-1) y)     


 minsertion  table = pure insertAtIndex <*>
                                 (putStrLn "Introduceti datele :" >>
                                 (mt >>= \t -> fmap transf $ return t )) <*>
                                 (putStr "La indexul.." >> (
                                  fmap (\e -> case e of
                                              (Just n) -> n
                                              _ -> -1 )
                                 ( fmap (fmap mtoInt) $ (getLine >>= \n -> if verify n then 
                                                                       return (Just n)
                                                                      else return Nothing ) )))  <*>
                                     pure table  
                                
 ---insertAtIndex :: Triple -> Int -> HsType -> HsType
 

 subtable :: (Int -> HsType -> HsType)->
                    HsType -> IO HsType
 subtable fn table = pure fn <*> 
                      (fmap mtoInt $ getLine) <*> 
                      pure table 
 --takeFirst --droptFirst --removeAtIndex dropLast



-------------------------------------------------

 filterallkeys :: HsType-> [Key]
 filterallkeys EndPointr = []
 filterallkeys (Row x y) = ((s1 $ t) x) : (filterallkeys y)

 filterallnames :: HsType -> [Name]
 filterallnames EndPointr = []
 filterallnames (Row x y)  = ((s2 $ t) x) : (filterallnames y)

 filteralljobs :: HsType -> [Job]
 filteralljobs EndPointr = []
 filteralljobs (Row x y)  = ((s3 $ t) x) : (filteralljobs y) 
  
 filterkeys :: (Key -> Bool) -> HsType -> [Key]
 filterkeys p table = filter p . filterallkeys $ table

 filternames :: (String -> Bool) -> HsType -> [Name]
 filternames p table = filter p . filterallnames $ table

 filterjobs :: (Job -> Bool) -> HsType -> [Job]
 filterjobs p table = filter p . filteralljobs $ table
-------------------------------------------------------------------
