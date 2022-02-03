module Agents where
import Utils

------------------------------------------------------------------------------------------
----                                       Niño                                       ----
------------------------------------------------------------------------------------------

------------------------- METODOS AUXILIARES DE NIñOS -----------------

-- buscar pos del niño
search_child_dir :: Pos -> [Child] -> Int
search_child_dir pos childs = snd (child_pos_search pos childs)

-- buscar niño por posicion
child_pos_search :: Pos -> [Child] -> Child
child_pos_search _ [] = ((-1, -1), -1)
child_pos_search pos (child: xr) | pos == (fst child) = child
                                  | otherwise = child_pos_search pos xr

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
mov_obj :: Pos -> Pos -> [Pos] -> [Pos]
mov_obj p1 p2 objects = concat_list p2 (list_without_elems p1 objects)

move_object :: Pos -> [Pos] -> [Pos] -> Int -> Int -> Int -> (Pos, [Pos])
move_object pos objects childs dir n m = if in_range (get_surr_by_dir pos dir objects) && is_not_dirt (get_surr_by_dir pos dir objects) && is_not_robot ((get_surr_by_dir pos dir objects))
                                then ((mov_dir pos dir), ( mov_obj (mov_dir pos dir) (get_surr_by_dir pos dir objects)))
                                else (pos, objects)

-- current Pos, direction, childs list, dirt list, n, m -> (next Pos , newObj, newChild, newDirt)
mv_child :: Pos -> Int -> [Child] -> [Pos] -> [Pos] -> Int -> Int -> (Pos, [Pos], [Child], [Pos])
mv_child pos dir childs dirt robots objects n m = if (is_not_dirt pos dir dirt) && (is_not_bbppen pos dir babypen) && (is_not_robot pos dir robots) 
                                                then (if is_obj pos dir objects
                                                        then (mov_dir pos dir, ((snd (move_object pos objects childs dir n m)), (concat_list ((mov_dir pos dir), dir) (list_without_elems (pos, dir) childs))), dirt)
                                                        else (mov_dir pos dir, objects, (concat_list ((mov_dir pos dir), dir) (list_without_elems (pos, dir) childs))), dirt)
                                                    )
                                                else (pos, objects, childs, dirt)



