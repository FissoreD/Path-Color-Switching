# Path-Color-Switching
 Ter supervised by Mr Jean-Charles Régin

The goal of this repository is to find some answers to the problems proposed in [this](./readme/Problem%20Description.pdf) file.

The report can be found [here](report/.aux/main.pdf).

The project in implemented in OCaml (version $4.13.1$) and dune (version $3.4.1$).

You can launch the main of the program through the command `dune exec -- TER`

List of optional arguments in command line:
```
-length   Set the length of path (deafult 10).
-src      Set the source of the path (default 1).
-v        Print each step of the algorithm execution.
-p        Print the list of node joinable in -length steps.
-allDiff  Set the allDifferent constraint.
-c        Count the number of paths from the source
-pFath    Print the fathers list, if in verbose mode.
-f        Set the file path to read the input to parse.
-loop     The algorithm computes paths from 1 to -length
-help     Display this list of options
--help    Display this list of options
  ```