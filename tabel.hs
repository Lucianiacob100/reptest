 
 import Data.Monoid

 {- Constructing a table as a linked list of cells
   ___        ___                        ___________
  | * | ---> | * | --->           * ---> EndPointr  |
  |_|_|      |___|         . . . .  .    ___________|
    |
    |____(                             )
         ( (cheie, nume, loc de munca) )

  -}
 
 data Meserie = Brutar String 
               |Mecanic String 
               |PilotF1 String 
               |Electrician String 
               |Medic String
               |Nimic            deriving (Show,Eq)

 data HashTable a b  c = EndPointr | Cell (a,b,c) (HashTable a b c) 
                            deriving (Show, Eq) 

 type Key = Int
 type Name = String
 type Triple = (Key,Name,Meserie)
 type HsType = HashTable Key String Meserie --declaring type synonyms 

 mytable :: HsType
 mytable = Cell (1, "Lucian", Brutar "de cativa ani") 
            ( Cell (2, "Ionut", PilotF1 "in timpul liber")
              ( Cell (3 ,"Marian", Mecanic "categoria 1") 
                ( Cell (4, "Cristian",Electrician "priceput")
                   ( Cell (3 ,"Marian", Mecanic "categoria 1")
                     (Cell (1, "Lucian", Brutar "de cativa ani") 
                        (Cell (5, "Mihai" , Medic "oftalmolog")( EndPointr)
                                           ))))))

 nilTriple :: Triple
 nilTriple = (-1 , "" ,Nimic )

 -- defining selectors

 first :: Triple -> Key
 first  = (\(x,y,z) -> x) 

 sec :: Triple -> Name
 sec = (\(x,y,z) -> y)

 thrd :: Triple -> Meserie
 thrd = (\(x,y,z) -> z) 
--
 class Nrcells a where
     nrOfCells :: a -> Int

 class Restoflist a where 
     restOfList :: a -> a

 class Elemente a where
     triv :: a -> a


 instance Elemente Int where
    triv n = n

 instance Elemente ([] a) where
    triv [s] = [s]

 instance Elemente Meserie where
     triv _ = Brutar ""

 instance Elemente (HashTable a b c) where
     triv EndPointr = EndPointr
     triv (Cell x y) = (Cell x y) 

 instance Nrcells (HashTable x y z) where
     nrOfCells EndPointr = 0
     nrOfCells (Cell x y) =  1 + (nrOfCells y)
 
 instance Restoflist (HashTable k n w) where
      restOfList (Cell x y) = y
      restOfList EndPointr = EndPointr
 
 firstCell :: HsType -> Triple
 firstCell (Cell x y) = x
 firstCell EndPointr = nilTriple

 getTriPos :: Int -> HsType -> Triple
 getTriPos 0 table = firstCell table
 getTriPos n (Cell x y) = getTriPos (n-1) y
 getTriPos n EndPointr = nilTriple

 searchfortrip :: Triple -> HsType -> Int 
 searchfortrip trip EndPointr = 1       
 searchfortrip trip (Cell x y) | trip == x = 0
                               | otherwise = 1 + searchfortrip trip y

 tripleexists :: Triple -> HsType -> Bool
 tripleexists trip table | n == l = False
                         |otherwise = True
         where n = searchfortrip trip table
               l = nrOfCells table 
 
 
 searchfordup :: Int-> Int -> Triple -> HsType -> (Int, Int)
 searchfordup _ _ _ EndPointr = (0,-1)
 searchfordup startPos itPos trip (Cell x y) | trip ==  mytrip = (startPos,itPos)
                                             |otherwise = searchfordup startPos (itPos + 1) trip y
                     where mytrip = firstCell y 


 duplicateentries  :: Int -> Int -> HsType -> [(Key,Key)]
 duplicateentries  i j (Cell x y )| y == EndPointr = [(0,-1)]
 duplicateentries i j (Cell x y) = (searchfordup i j x (Cell x y)) : (duplicateentries (i+1) (j+1) y)
  
 onlyduplicates :: HsType -> [(Key,Key)] 
 onlyduplicates  table = filter (/= (0,-1) ) (duplicateentries 0 1 table)

 mapovert :: (Triple -> Triple ) -> HsType -> HsType
 mapovert f EndPointr = EndPointr
 mapovert f (Cell x y) = Cell (f x) (mapovert f y)

--filter f applied over his list of cells
 filterovert :: (Triple -> Bool) -> HsType -> HsType
 filterovert p EndPointr = EndPointr
 filterovert p (Cell x y) | (p x) =  Cell x (filterovert  p y)
                          |otherwise = filterovert p y

 removedup :: HsType -> HsType                             
 removedup   EndPointr = EndPointr  
 removedup (Cell x y) = let
    filterbyfirst :: HsType -> HsType
    filterbyfirst EndPointr = EndPointr
    filterbyfirst (Cell  x y) = Cell x (filterovert (/= x) y)
             in  filterbyfirst  (Cell x (removedup  y))

---------------------------------------------------------------
---------------------------------------------------------------
 searchforkey :: Key -> HsType -> Int --returns an index
 searchforkey key (Cell x y) |(y == EndPointr) && ((first x) /= key) = 1
                             |key == (first x) = 0
                             |otherwise = 1 + searchforkey key y

 keyexists :: Key -> HsType -> Bool
 keyexists key table | n == l = False
                     |otherwise = True
          where n = searchforkey key table
                l = nrOfCells table

 searchforname :: Name -> HsType -> Int  
 searchforname name (Cell x y) | (y == EndPointr) && ((sec x) /= name) = 1
                               | name == (sec x) = 0
                               | otherwise = 1 + searchforname name y 
 
 nameexists :: Name -> HsType -> IO ()
 nameexists name table | n == l = putStrLn "Numele nu exista in tabel"
                       |otherwise = putStrLn ("Numele se afla in tabel la index " ++ (show n))
           where n = searchforname name table
                 l = nrOfCells table
 -------------------------------------------------------------------
---------------------------------------------------------------------

 searchForElem :: (Elemente a, Eq a) => a -> (Triple -> a ) -> HsType -> Maybe Triple
 searchForElem e sel EndPointr = Nothing
 searchForElem e sel (Cell x y) = (Just e) >>= 
                                 \el -> (Just (sel x)) >>=
                                 \ en -> if el == en then 
                                         return x
                                         else (searchForElem e sel y) 
                                                      

-------------------------------------------------------------------------
-------------------------------------------------------------------------
--get the key from a triple at nth position
 getKey :: Int ->  HsType -> Key 
 getKey n table = first . getTriPos n $ table

 getName :: Int -> HsType -> Name
 getName n table = sec . getTriPos n $ table

 getJob :: Int -> HsType -> Meserie
 getJob n table = thrd . getTriPos n $ table

------------------------------------------------------------------------------
     
 fltFirst :: Elemente a =>  Int ->  HsType-> (Triple -> a) -> [a]
 fltFirst n table selec = (firstntup n table) >>= \ e -> [(selec e)]
-----uses the filterall functions

 printElm :: Show a => [a] -> IO ()
 printElm [] = print ""
 printElm (x:xs) = return (x:xs) >>= \s -> print x >> printElm xs

 printRows :: [(Triple)] -> IO()
 printRows [] = putStr "\n"
 printRows (x:xs) = return (x:xs) >>= 
                    \s -> putStr (show (first x)) >>
                    putStr "\t" >>
                    putStr (sec x) >> 
                    putStr "\t" >>
                    putStr (show (thrd x)) >>
                    putStr "\n" >>
                    printRows xs

 printSelected :: Show a => Int-> HsType -> (Triple -> a) -> IO ()
 printSelected n table selec =  printElm $ ((firstntup n table) >>= \e -> [(selec e)]) 
                            
 printAll :: Show a => (t -> [a]) -> t -> IO ()
 printAll  filterf table =  putStrLn ("Elementele slectate din tabel sunt: \n" <>     
                                  (mconcat  
                                    ((fmap show $ (filterf table)) >>=                       
                                      (\x -> [x <> "\n"]))))                              
--prints all  elements each one on a row

 mtoInt ::  String ->  Int
 mtoInt = \x -> (read x :: Int)

 makeTriple :: Key -> Name -> Meserie -> Triple
 makeTriple k n m =  (k,n,m) 

 --given a Meserie, makes a new IO Triple 
 makeTrip :: Meserie -> IO (Int,String,Meserie)
 makeTrip msr = getLine >>= \k -> getLine >>= \n -> return ((mtoInt k) , n , msr) 


 mAddElem :: (Triple -> HsType -> HsType) ->
               Meserie -> HsType -> IO HsType
 mAddElem addf t_msr table = pure addElemB <*> 
                            (makeTrip t_msr) <*>
                             pure table
 --addElemB --addElemE --insert at beginning or the end


 minsertion :: (Triple -> Int -> HsType -> HsType) ->
                        Meserie -> HsType -> IO HsType
 minsertion insrtf t_msr table = pure insrtf <*>
                                (makeTrip t_msr) <*>
                                (fmap mtoInt $ getLine) <*>
                                pure table
 ---insertAtIndex :: Triple -> Int -> HsType -> HsType

 

 subtable :: (Int -> HsType -> HsType) ->
                    HsType -> IO HsType
 subtable fn table = pure fn <*> 
                      (fmap mtoInt $ getLine) <*> 
                      pure table 
 --takeFirst --droptFirst --removeAtIndex dropLast

 mgetElem :: Elemente a => ( Int ->  HsType -> a) ->
                                     HsType -> IO a              
 mgetElem fn table = pure fn <*> 
                     ( fmap mtoInt $ getLine) <*>
                      pure table 
 --getKey --getName --getJob

-------------------------------------------------

 filterallkeys :: HsType-> [Key]
 filterallkeys EndPointr = []
 filterallkeys (Cell x y) = (first x) : (filterallkeys y)

 filterallnames :: HsType -> [Name]
 filterallnames EndPointr = []
 filterallnames (Cell x y)  = (sec x) : (filterallnames y)

 filteralljobs :: HsType -> [Meserie]
 filteralljobs EndPointr = []
 filteralljobs (Cell x y)  = (thrd x) : (filteralljobs y) 
  
 filterkeys :: (Key -> Bool) -> HsType -> [Key]
 filterkeys p table = filter p . filterallkeys $ table

 filternames :: (String -> Bool) -> HsType -> [Name]
 filternames p table = filter p . filterallnames $ table

 filterjobs :: (Meserie -> Bool) -> HsType -> [Meserie]
 filterjobs p table = filter p . filteralljobs $ table
-------------------------------------------------------------------
 firstntup :: Int -> HsType -> [Triple]
 firstntup 0 (Cell x y) = [x]
 firstntup n (Cell x y) = [x] ++ (firstntup (n-1) y)
 firstntup n EndPointr = [nilTriple]


 takeFirst :: Int -> HsType -> HsType
 takeFirst _ EndPointr = EndPointr
 takeFirst ind (Cell x y) | ind == 0 = EndPointr
                          |otherwise = (Cell x (takeFirst (ind - 1) y)) 


 dropFirst :: Int -> HsType -> HsType
 dropFirst _ EndPointr   = EndPointr
 dropFirst ind table      | ind == 0  = table
 dropFirst ind (Cell x y) | ind == 1 = y
                          |otherwise = dropFirst (ind-1) y  

 --"remove" the last element
 removeElemE :: HsType -> HsType
 removeElemE (Cell x y) | y == EndPointr = EndPointr
                        |otherwise = Cell x (removeElemE y)
 
 -- drop last n elemnts
 dropLast :: Int -> HsType -> HsType
 dropLast _ EndPointr = EndPointr
 dropLast ind table  | ind == 0 = table
                     |otherwise = dropLast (ind - 1) (removeElemE table)
--runs very slowly for large table lists

 slicetable :: HsType ->Int  -> Int -> Maybe HsType
 slicetable  _ ind1 ind2          | ind1 > ind2 = Nothing
 slicetable  (Cell x y) ind1 ind2 | ind1 == ind2 = Just (Cell x EndPointr)
                                  |otherwise = Just ((dropFirst ind1) . 
                                                 (dropLast (l - ind2))  $ (Cell x y))
               where l = nrOfCells (Cell x y)


 addElemB :: Triple -> HsType -> HsType
 addElemB trip (Cell x y) = Cell trip (Cell x y)
 --will result in a new table with the new element at the beginning

 addElemE :: Triple -> HsType -> HsType
 addElemE trip EndPointr = Cell trip EndPointr
 addElemE trip (Cell x y) | y == EndPointr =  (Cell x (Cell trip EndPointr))
                          |otherwise = Cell x (addElemE trip y)             


 --joining two tables
 tablejoin :: HsType -> HsType -> HsType
 tablejoin (Cell x y) table | y == EndPointr =  (Cell x table) 
                            |otherwise       = Cell x (tablejoin y table)



 --reversing the order of the cells
 reversetable :: HsType -> HsType
 reversetable (Cell x y)| y == EndPointr = (Cell x y)
                        |otherwise       = addElemE x (reversetable y) 


 insertAtIndex :: Triple -> Int -> HsType -> HsType
 insertAtIndex trip nPos EndPointr   = EndPointr
 insertAtIndex trip nPos (Cell x y) | nPos == 0 = Cell trip (Cell x y) 
                                    | nPos == 1 = Cell x (Cell trip y)
                                    | otherwise = Cell x (insertAtIndex trip (nPos-1) y) 

 
 removeAtIndex :: Int -> HsType -> HsType
 removeAtIndex nPos EndPointr = EndPointr
 removeAtIndex nPos (Cell x y) | nPos == 0 = y
                               |otherwise = Cell x (removeAtIndex (nPos-1) y)   



