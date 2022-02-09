module Agents where
import Utils

------------------------- METODOS AUXILIARES -----------------

-- pos a la que me voy a mover
is_next_obj :: Pos -> Int -> [Pos]  -> Bool
is_next_obj pos dir list = elem (mov_dir pos dir) list

-- posicion del siguiente objeto en esa direccion
get_surr_by_dir :: Pos -> Int -> [Pos] -> Pos
get_surr_by_dir pos dir elems | is_next_obj pos dir elems = get_surr_by_dir (mov_dir pos dir) dir  elems
                             | otherwise = (mov_dir pos dir)

--------------------------- METODOS BOOLEANOS --------------------------

is_obj :: Pos -> Int -> [Pos] -> Bool
is_obj pos dir list = elem (mov_dir pos dir) list

is_not_robot :: Pos -> Int -> [Pos] -> Bool
is_not_robot pos dir list = notElem (mov_dir pos dir) list

is_not_dirt :: Pos -> Int -> [Pos] -> Bool
is_not_dirt pos dir list = notElem (mov_dir pos dir) list

is_not_bbppen :: Pos -> Int -> [Pos] -> Bool
is_not_bbppen pos dir list = notElem (mov_dir pos dir) list


------------------------------- METODOS DE MOVIMIENTO ----------------------

-- lista de objetos actualizada
move_object__ :: Pos -> Pos -> [Pos] -> [Pos]
move_object__ p1 p2 objects = p2 : (list_without_elems p1 objects)

move_object :: Pos -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> Int -> Int -> Int -> (Pos, [Pos])
move_object pos objects childs dirt robots i n m = if (in_range (get_surr_by_dir pos i objects) n m) && (notElem (get_surr_by_dir pos i objects) dirt) && (notElem (get_surr_by_dir pos i objects) robots)
                                                    then ((mov_dir pos i), ( move_object__ (mov_dir pos i) (get_surr_by_dir pos i objects) objects))
                                                    else (pos, objects)

-- movimiento del niño
mov_child :: Pos -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> Int -> Int -> (Pos, [Pos], [Pos], [Pos])
mov_child pos childs dirt robots objects babypen n m = mov_child__ pos (get_rand 0 3) childs dirt robots objects babypen n m (val_pos (not_in_list (not_in_list (not_in_list (not_in_list (get_surr_grid pos) dirt) robots) objects) babypen) n m) 

mov_child__ :: Pos -> Int -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> Int -> Int -> [Pos] -> (Pos, [Pos], [Pos], [Pos])
mov_child__ pos i childs dirt robots objects babypen n m grid = let next = mov_dir pos i
                                                                in ( if in_range next n m && (notElem next childs) && (notElem next dirt) && (notElem next robots) && (notElem next babypen) 
                                                                    then ( 
                                                                        if (elem next objects)
                                                                            then (let (mov, obj) = move_object pos objects childs dirt robots i n m
                                                                                in (mov, obj, mov:(list_without_elems pos childs), dirt))
                                                                            else ( next, objects, (next:(list_without_elems pos childs)), 
                                                                                (if (get_rand 1 100) < 40 && (length (not_in_list (pos:(grid)) [next]) > 0)
                                                                                    then (get_rand_elem_from_list 1 (not_in_list (pos:(grid)) [next]))++dirt
                                                                                    else dirt)))
                                                                    else (pos, objects, childs, (if (get_rand 1 100) < 20 && (length (not_in_list (pos:(grid)) [next]) > 0) 
                                                                                                    then (get_rand_elem_from_list 1 grid)++dirt 
                                                                                                    else dirt)))



-- movimiento del robot
mov_robot :: Pos -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos]  -> [Pos] -> Int -> Int -> (Pos, [Pos], [Pos], [Pos], [Pos], [Pos], [Pos])
mov_robot pos childs dirt objects babypen babypenused robots robchi n m =  if (elem pos robchi)
                                                                                then (if (elem pos babypen)
                                                                                        then(pos, robots, [], childs, dirt, (list_without_elems pos babypen), (pos : babypenused))
                                                                                        else ( let next = (breadth_first_search pos pos [] [[pos, pos]] babypen (objects++babypenused) n m)!!0
                                                                                                in (if next == (-1,-1)
                                                                                                        then (pos, robots, robchi, childs, dirt, babypen, babypenused)
                                                                                                        else (next, [next], [next], childs, dirt, babypen, babypenused))))
                                                                                else (  if (length childs) > 0 
                                                                                            then (  if (elem pos childs)
                                                                                                        then(pos, [pos], [pos], (list_without_elems pos childs), dirt, babypen, babypenused )
                                                                                                        else(let next = (breadth_first_search pos pos [] [[pos, pos]] childs (objects++babypenused) n m)!!0
                                                                                                                in (if next == (-1,-1)
                                                                                                                    then (pos, robots, robchi, childs, dirt, babypen, babypenused)
                                                                                                                    else (next, [next], robchi, childs, dirt, babypen, babypenused) ) ) )
                                                                                            else ( if (length dirt) > 0
                                                                                                    then (  if (elem pos dirt)
                                                                                                                then (pos, robots, robchi, childs, (list_without_elems pos dirt), babypen, babypenused)
                                                                                                                else (let next = (breadth_first_search pos pos [] [[pos, pos]] dirt (objects++babypenused) n m)!!0
                                                                                                                        in (if next == (-1,-1)
                                                                                                                            then (pos, robots, robchi, childs, dirt, babypen, babypenused)
                                                                                                                            else (next, [next], robchi, childs, dirt, babypen, babypenused) ) ) )
                                                                                                    else (pos, robots, robchi, childs, dirt, babypen, babypenused)))

-- corral
create_playpen :: Int -> Int -> Int -> Int  -> Pos -> [Pos]
create_playpen dir i n m (x, y) | i == 0 || not (in_range (x, y) n m) = []
                                | dir == 0 = (x, y) : create_playpen dir (i-1) n m (mov_dir (x, y) 1) 
                                | otherwise = (x, y) : create_playpen dir (i-1) n m (mov_dir (x, y) 0)