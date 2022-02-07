module Utils where
import System.IO.Unsafe                                         
import System.Random 

type Pos = (Int, Int)
type Child = (Pos, Int)

-------- METODOS AUXILIARES --------------

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
                            else not_in_list xr list
 
-- crea una lista con los primeros elementos de cada lista que contiene
get_init :: [[Pos]] -> [Pos]
get_init [] = []
get_init (x:xr) = x!!0 : get_init xr 

-- elimina un elemento de una lista
lwoe _ list [] = list
lwoe i list (x:xr) = if i == x
                                    then list ++ xr
                                    else lwoe i (concat_list x list) xr
list_without_elems i list = lwoe i [] list                           


get_rand_elem_from_list :: Int -> [Pos] -> [Pos]
get_rand_elem_from_list i list = get_rand_elem_from_list__ 0 i (length list) list

get_rand_elem_from_list__ :: Int -> Int -> Int -> [Pos] -> [Pos]
get_rand_elem_from_list__ current n total list 
        | current == n = []
        | otherwise = let x = list!!(get_rand 0 (total - current - 1))
                        in x : get_rand_elem_from_list__ (current + 1) n total (list_without_elems x list)


get_surr_grid :: Pos -> [Pos]
get_surr_grid (x, y) = [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]










create_matrix :: Int -> Int -> Int -> Int -> [Pos]
create_matrix x y xt yt  | (x /= xt && y /= yt) = (x, y) : create_matrix x (y+1) xt yt
                            |  y == yt = create_matrix (x + 1) 0 xt yt
                            |  x == xt = [] 
                            |  otherwise = []

 
list_from_string :: [String] -> Int -> [String]
list_from_string [] _ = []
list_from_string list n = (unwords (take n list)) : list_from_string (drop n list) n


print_string_list :: [String] -> IO ()
print_string_list [] = putStr ""
print_string_list (x:xr) = do 
                    putStrLn x
                    print_string_list xr

print_field :: [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> Int -> Int -> IO ()
print_field robots dirt child objects babypen robotbaby n m = print_string_list (list_from_string (tail (create_list (0,0) (create_matrix 0 0 n m) robots dirt child objects babypen robotbaby)) m)



----------- METODOS PARA EL breadth_first_search ------------

-- returns if a position does not go off the board
in_range (x, y) xt yt = x >= 0 && y >=0 && x < xt && y < yt

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


-- buscar el mas cercano al inicio en el camino del breadth_first_search
find_closest :: Pos -> [Pos] -> [[Pos]] -> Pos
find_closest destiny current elems | (current!!1) == destiny = current!!0
                               | otherwise = find_closest destiny (looking_for_father (current!!1) elems) elems

rel_chi_father x y = concat_list y (concat_list x [])

-- elemento con su padre
asosiate_father :: [Pos] -> Pos -> [[Pos]]
asosiate_father [] _ = []
asosiate_father (x: xr) ch = (rel_chi_father x ch) : asosiate_father xr ch

-- BFS
breadth_first_search :: Pos -> Pos -> [[Pos]] -> [[Pos]] -> [Pos] -> [Pos] -> Int -> Int-> [Pos]
breadth_first_search _ _ _ [] _ _ _ _ = [(-1,-1), (-1, -1)]
breadth_first_search init pos seen queue elements notemty n m  
    | elem pos elements =  concat_list pos (concat_list (find_closest init (head queue) seen) []) 
    | otherwise = breadth_first_search init 
        ((head ((tail queue)++(asosiate_father (not_in_list (not_in_list (val_pos (find_surrounding pos) n m) (get_init seen)) notemty) pos)))!!0) 
        ((head queue): seen) --
        ((tail queue)++(asosiate_father (not_in_list (not_in_list (val_pos (find_surrounding pos) n m) (get_init seen)) notemty) pos)) 
        elements
        notemty
        n m
