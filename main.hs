module Main where
import Board
import Utils
import Printer
import Text.Printf

init = main

main = do
    let turn = (get_rand 5 36)
    let n = (get_rand 3 10)
    let m = (get_rand 3 10)
    let board = new_board n m
    printf "\n♥☻ Leyenda ☻♥:\nrobot _R_\nsuciedad: _d_\nniño: _c_\nobjeto: _o_\ncorral: _b_\nrobot con niño: _X_\ncorral con niño: _B_\nvacío: _-_\n"
    printf "\nTurnos:%d\n" turn
    printf "Tamaño del tablero: \nfilas: %d, \ncolumnas: %d\n\n" n m
    putStrLn (sim board 0 turn)

sim board@(Board height_ width_ robots_ childs_ robotsWithChild_ object_ dirt_ babypen_ babypenused_) cturn turn = 
    if (cturn == turn) 
        then (
            let perc = (floor (realToFrac((60 * height_ * width_) `div` 100)))
            in (if ((length dirt_ ) < perc) then "Se logro el objetivo!\n\n" else "NO se logro el objetivo :("))
        else let newboard = (mv_agents board) in "Turno: "++(show cturn)++"\n"++(board_to_string newboard)++(sim newboard (cturn+1) turn)
      