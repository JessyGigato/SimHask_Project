module Printer where
import Utils

--- PRINTER ---
dictionar :: Int -> String
dictionar x | x == 0 = "(R)" -- robot
            | x == 1 = "(d)" -- suciedad
            | x == 2 = "(c)" -- nino
            | x == 3 = "(o)" -- objeto
            | x == 4 = "(b)" -- corral
            | x == 5 = "(X)" -- robot con nino
            | x == 6 = "(B)" -- corral usado
            | x == 7 = "( )" -- vacio

create_list :: Pos -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [Pos] -> [String]
create_list current [] robots dirt child objects babypen babypenused robotbaby 
    | elem current robots      = [dictionar 0]  
    | elem current dirt        = [dictionar 1] 
    | elem current child       = [dictionar 2] 
    | elem current objects     = [dictionar 3]
    | elem current babypen     = [dictionar 4] 
    | elem current robotbaby   = [dictionar 5] 
    | elem current babypenused = [dictionar 6] 
    | otherwise                = [dictionar 7] 
create_list current (x:xr) robots dirt child objects babypen babypenused robotbaby 
    | elem current robots        = dictionar 0 : create_list x xr robots dirt child objects babypen robotbaby
    | elem current dirt          = dictionar 1 : create_list x xr robots dirt child objects babypen robotbaby
    | elem current child         = dictionar 2 : create_list x xr robots dirt child objects babypen robotbaby
    | elem current objects       = dictionar 3 : create_list x xr robots dirt child objects babypen robotbaby
    | elem current babypen       = dictionar 4 : create_list x xr robots dirt child objects babypen robotbaby
    | elem current robotbaby     = dictionar 5 : create_list x xr robots dirt child objects babypen robotbaby
    | elem current robotbabyused = dictionar 6 : create_list x xr robots dirt child objects babypen robotbaby
    | otherwise                  = dictionar 7 : create_list x xr robots dirt child objects babypen robotbaby
