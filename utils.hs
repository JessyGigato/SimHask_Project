module Utils where
import System.IO.Unsafe                                         
import System.Random 

type Pos = (Int, Int)
type Child = (Pos, Int)

-------- METODOS AUXILIARES --------------

-- generar un numero random en un intervalo
get_rand :: Int -> Int -> Int 
get_rand min max = unsafePerformIO (getStdRandom (randomR (min, max)))

-- devuelve una lista donde no estan los elementos de la 1era
not_in_list :: [Pos] -> [Pos] -> [Pos]
not_in_list [] _ = []
not_in_list (x:xr) list = if notElem x list 
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
                        else lwoe i (x : list) xr
list_without_elems i list = lwoe i [] list                           

concat_list x [] = x : []
concat_list x (x1:xs) = x1 : concat_list x xs

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



----------- METODOS PARA EL breadth_first_search ------------

-- returns if a position does not go off the board
in_range (x, y) xt yt = x >= 0 && y >=0 && x < xt && y < yt

-- aleda??os a una pos (sur, este, norte, oeste)
mov_dir :: Pos -> Int -> Pos
mov_dir (x, y) i  | i == 0 = (x+1, y) 
                    | i == 1 = (x, y+1) 
                    | i == 2 = (x-1, y) 
                    | i == 3 = (x, y-1) 

-- todos los aleda??os  
find_surrounding (x, y) = [(mov_dir (x, y) 0), (mov_dir (x, y) 1), (mov_dir (x, y) 2), (mov_dir (x, y) 3)]  
                        
-- posiciones validas
val_pos :: [Pos] -> Int -> Int -> [Pos]
val_pos [] _ _ = []
val_pos (ch : xr) n m = if in_range ch n m then ch : val_pos xr n m else  val_pos xr n m

looking_for_father :: Pos -> [[Pos]] -> [Pos]
looking_for_father _ [] = [(-1, -1), (-1, -1)] 
looking_for_father ch (x:xr) = if ch == (x!!0) then x else looking_for_father ch xr

-- buscar el mas cercano al inicio en el camino del breadth_first_search
find_closest :: Pos -> [Pos] -> [[Pos]] -> Pos
find_closest destiny current elems | (current!!1) == destiny = current!!0
                               | otherwise = find_closest destiny (looking_for_father (current!!1) elems) elems

rel_chi_father x y = [x, y]

-- elemento con su padre
asosiate_father :: [Pos] -> Pos -> [[Pos]]
asosiate_father [] _ = []
asosiate_father (x: xr) ch = (rel_chi_father x ch) : asosiate_father xr ch

-- BFS
breadth_first_search :: Pos -> Pos -> [[Pos]] -> [[Pos]] -> [Pos] -> [Pos] -> Int -> Int-> [Pos]
breadth_first_search _ _ _ [] _ _ _ _ = [(-1,-1), (-1, -1)]
breadth_first_search init pos seen queue elements notemty n m = 
    if elem pos elements
        then pos : [find_closest init (head queue) seen]
        else breadth_first_search 
            init 
            ((head ((tail queue)++
                (asosiate_father (not_in_list (not_in_list (val_pos (find_surrounding pos) n m) 
                    (get_init seen)) notemty) pos)))!!0) 
            ((head queue): seen) 
            ((tail queue)++
                (asosiate_father (not_in_list (not_in_list (val_pos (find_surrounding pos) n m) 
                    (get_init seen)) notemty) pos)) 
            elements
            notemty
            n
            m