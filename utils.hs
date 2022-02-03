module Utils where
import System.IO.Unsafe                                         
import System.Random 

type Pos = (Int, Int)
type Child = (Pos, Int)

-------------------------------------------------------------------------------------
-------- METODOS AUXILIARES ---------------
-- generar un numero random en un intervalo
get_rand :: Int -> Int -> Int 
get_rand min max = unsafePerformIO (getStdRandom (randomR (min, max)))

-- concatena un elemento a una lista
concat_list x [] = x : []
concat_list x (x1:xs) = x1 : concat_list x xs

-- lista de tamano n con x elementos iguales
n_eq_list _ 0 = []
n_eq_list x n = x : n_eq_list x (n - 1)

-- calculates the percentage of a number x with respect to a total y
percent perc total = perc * total / 100

-- devuelve una lista donde no estan los elementos de la 1era
not_in_list :: [Pos] -> [Pos] -> [Pos]
not_in_list [] _ = []
not_in_list (x:xr) list = if x `notElem` list 
                            then x : not_in_list xr list 
 
-- crea una lista con los primeros elementos de cada lista que contiene
get_init :: [[Pos]] -> [Pos]
get_init [] = []
get_init (x:xr) = x!!0 : get_init xr 

-- elimina un elemento de una lista
lwoe _ list [] = list
lwoe i list (x:xr) = if i == x
                                    then list ++ xr
                                    else lwoe i (concat_list x list) xr
list_without_elems i list = lwoe i [] list                           else not_in_list xr list

-------------------------------------------------------------------------------------
----------- METODOS PARA EL BFS ------------
-- returns if a position does not go off the board
in_range (x, y) totalx totaly = x >= 0 && y >=0 && x < totalx && y < totaly

-- aledaños a una pos 
mov_dir :: Pos -> Int -> Pos
mov_dir (x, y) i  | i == 0 = (x+1, y) -- south
                        | i == 1 = (x, y+1) -- east
                        | i == 2 = (x-1, y) -- north
                        | i == 3 = (x, y-1) -- west
            
find_surrounding (x, y) = concat_list (mov_dir (x, y) 3) (concat_list (mov_dir (x, y) 2) (concat_list (mov_dir (x, y) 1) (concat_list (mov_dir (x, y) 0) [])))  
                        
-- posiciones validas
val_pos :: [Pos] -> Int -> Int -> [Pos]
val_pos [] _ _ = []
val_pos (ch : xr) n m  | in_range ch n m = ch : val_pos xr n m
                        | otherwise = val_pos xr n m

looking_for_father :: Pos -> [[Pos]] -> [Pos]
looking_for_father _ [] = [(-1, -1), (-1, -1)] -- break
looking_for_father ch (x:xr) | ch == (x!!0) = x
                        | otherwise = looking_for_father ch xr


-- buscar el mas cercano al inicio en el camino del bfs
find_closest :: Pos -> [Pos] -> [[Pos]] -> Pos
find_closest destiny current elems | (current!!1) == destiny = current!!0
                               | otherwise = find_closest destiny (looking_for_father (current!!1) elems) elems


rel_chi_father x y = concat_list y (concat_list x [])

-- elemento con su padre
asosiate_father :: [Pos] -> Pos -> [[Pos]]
asosiate_father [] _ = []
asosiate_father (x: xr) ch = (rel_chi_father x ch) : asosiate_father xr ch

-- bfs
breadth_first_search :: Pos -> Pos -> [[Pos]] -> [[Pos]] -> [Pos] -> Int -> Int-> [Pos]
breadth_first_search _ _ _ [] _ _ _ = [(-1,-1), (-1, -1)]
breadth_first_search init pos seen queue elements n m  | pos `elem` elements =  concat_list pos (concat_list (find_closest init (head queue) seen) []) 
                            | otherwise = breadth_first_search init 
                                ((head ((tail queue)++(asosiate_father (not_in_list (val_pos (find_surrounding pos) n m) (get_init seen)) pos)))!!0) 
                                (concat_list (head queue) seen) --
                                ((tail queue)++(asosiate_father (not_in_list (val_pos (find_surrounding pos) n m) (get_init seen)) pos)) 
                                elements
                                n m
