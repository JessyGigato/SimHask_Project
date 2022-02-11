module Printer where
import Utils

--- PRINTER ---
dictionar :: Int -> String
dictionar x | x == 0 = "_X_" -- robot con nino
            | x == 1 = "_R_" -- robot
            | x == 2 = "_d_" -- suciedad
            | x == 3 = "_c_" -- nino
            | x == 4 = "_o_" -- objeto
            | x == 5 = "_b_" -- corral
            | x == 6 = "_B_" -- corral usado
            | x == 7 = "_-_" -- vacio

create_list :: Pos -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [String]
create_list current [] robots dirt child objects babypen babypenused robotbaby  
    | elem current robotbaby   = [dictionar 0]
    | elem current robots      = [dictionar 1]  
    | elem current dirt        = [dictionar 2] 
    | elem current child       = [dictionar 3] 
    | elem current objects     = [dictionar 4]
    | elem current babypen     = [dictionar 5] 
    | elem current babypenused = [dictionar 6] 
    | otherwise                = [dictionar 7] 
create_list current (x:xr) robots dirt child objects babypen babypenused robotbaby 
    | elem current robotbaby     = dictionar 0 : create_list x xr robots dirt child objects babypen babypenused robotbaby
    | elem current robots        = dictionar 1 : create_list x xr robots dirt child objects babypen babypenused robotbaby
    | elem current dirt          = dictionar 2 : create_list x xr robots dirt child objects babypen babypenused robotbaby
    | elem current child         = dictionar 3 : create_list x xr robots dirt child objects babypen babypenused robotbaby
    | elem current objects       = dictionar 4 : create_list x xr robots dirt child objects babypen babypenused robotbaby
    | elem current babypen       = dictionar 5 : create_list x xr robots dirt child objects babypen babypenused robotbaby
    | elem current babypenused   = dictionar 6 : create_list x xr robots dirt child objects babypen babypenused robotbaby
    | otherwise                  = dictionar 7 : create_list x xr robots dirt child objects babypen babypenused robotbaby