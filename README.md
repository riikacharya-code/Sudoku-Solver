This project can take in any full sudoku board and can solve it mostly, if not completely
To begin running the project, cd into this directory and open up emacs. 

This project containts two files you can compile and run. First is the hill-climbing file, and second is the simulated-annealing file.

Hill Climbing takes in a full sudoku board and incrementally makes changes to it until it is solved. Simulated Annealing takes in a full sudoku board and solves it similarly to Hill Climbing, but if it ends up with a worse solution than it had before, it may do a set of random moves.

To run the hill-climbing file, run the commands:
(compile-file "sudoku-solver-hill-climbing.lisp") (load "sudoku-solver-hill-climbing"). 
The test data is already there and each sudoku game before and after hill-climbing will be displayed.

Similarly, to run the simulated-annealing file, run the commands:
(compile-file "sudoku-solver-simulated-annealing.lisp") (load "sudoku-solver-simulated-annealing").


