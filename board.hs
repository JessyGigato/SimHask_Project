module Board where
import Utils
import Agents
import Printer
import Text.Printf


------------  TABLERO -----------------

data Board = Board { height :: Int, width :: Int,  robots :: [Pos], childs :: [Pos], robotchild :: [Pos], object :: [Pos], dirt :: [Pos], playpen :: [Pos], playpenused :: [Pos]}

instance Show (Board) where {
    show board = let Board { height = height_ , width = width_ ,  robots = robots_ ,  childs = childs_ , robotchild = robotchild_ , object = object_ , dirt = dirt_ , playpen = playpen_ , playpenused = playpenused_ } = board
        in printf "\nDimensión del tablero: \n%d filas, \n%d columnas\n\n%s" height_ width_ (print_field robots_ dirt_ childs_ object_ playpen_ playpenused_ robotchild_ height_ width_)}


list_to_string :: [String] -> Int -> [String]
list_to_string [] _ = []
list_to_string list n = (unwords (take n list)) : list_to_string (drop n list) n

print_string_list :: [String] -> String
print_string_list [] = "\n"
print_string_list (x:xr) = x++"\n"++(print_string_list xr)

print_field :: [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> Int -> Int -> String
print_field robots dirt child objects playpen playpenused robotbaby n m = print_string_list (list_to_string (tail (create_list (0,0) (create_matrix 0 0 n m) robots dirt child objects playpen playpenused robotbaby)) m)

board_to_string (Board height_ width_ robots_ childs_ robotchild_ object_ dirt_ playpen_ playpenused_) = 
    (print_field robots_ dirt_ childs_ object_ playpen_ playpenused_ robotchild_ height_ width_)

base n m =  Board { height = n, width = m, robots = [], childs = [], robotchild = [], object = [], dirt = [], playpen = [], playpenused = []}

new_board_temp_ :: Board -> [[Pos]]
new_board_temp_ (Board height_ width_ robots_ childs_ robotchild_ object_ dirt_ playpen_ playpenused_) = 
    do
        let rest_ = get_rand_elem_from_list (height_*width_) (create_matrix 0 0 height_ width_)
        let ppen = create_playpen (get_rand 1 2) (get_rand 1 ((min height_ width_)-1)) height_ width_ ((get_rand_elem_from_list 1 rest_)!!0)
        let rest = not_in_list rest_ ppen
        let ch = take (length ppen) rest --floor (realToFrac((20 * height_ * width_) `div` 100)
        let obj = take (get_rand 0 (floor (realToFrac((20 * height_ * width_) `div` 100)))) (drop ((length ch)*2) rest) 
        let dir = take (get_rand 0 (floor (realToFrac((20 * height_ * width_) `div` 100)))) (drop (((length ch)*2)+ (length obj)) rest)
        let rob = take 1 (drop (((length ch)*2) + (length obj) + (length dir)) rest)
        [ppen, ch, obj, dir, rob]

new_board_ :: Board -> [[Pos]] -> Board
new_board_ board list = 
        let Board { height = height_, width = width_, robots = robots_,  childs = childs_, robotchild = robotchild_, object = object_,  dirt = dirt_, playpen = playpen_, playpenused = playpenused_} = board
        in  Board { height = height_, width = width_, robots = list!!4, childs = list!!1, robotchild = robotchild_, object = list!!2, dirt = list!!3, playpen = list!!0, playpenused = playpenused_ } 

new_board n m = new_board_ (base n m) (new_board_temp_ (base n m))


mv_agents_temp_ (Board height_ width_ robots_ childs_ robotchild_ object_ dirt_ playpen_ playpenused_) = 
    do
        let (_ , newRobot, newRobotChild, newChild, newDirt, newbabypen, newbabyPenUsed) = mov_robot (robots_!!0) childs_ dirt_ object_ playpen_ playpenused_ robots_ robotchild_ height_ width_
        let (_ , newObj_, newChild_, newDirt_) = move_all_childs newChild newChild newDirt newRobot object_ newbabypen height_ width_
        [newRobot, newChild_, newRobotChild, newObj_, newDirt_, newbabypen, newbabyPenUsed]
    

mv_agents_ (Board height_ width_ robots_ childs_ robotchild_ object_ dirt_ playpen_ playpenused_) list = 
    Board { height = height_, width = width_, robots = list!!0, childs = list!!1, robotchild = list!!2, object = list!!3, dirt = list!!4, playpen = list!!5, playpenused = list!!6}

mv_agents board = mv_agents_ board (mv_agents_temp_ board)

move_all_childs [] childs_ dirt_ robots_ object_ playpen_ height_ width_ = (0, object_, childs_, dirt_)
move_all_childs (ch : chr) childs_ dirt_ robots_ object_ playpen_ height_ width_ = 
    let (_, obj, child, dirt) = mov_child ch childs_ dirt_ robots_ object_ playpen_ height_ width_
    in move_all_childs chr child dirt robots_ obj playpen_ height_ width_