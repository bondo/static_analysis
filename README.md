Static Analysis Q3 2013
=======================
This is an implementation of some of the stuff from the Static Analysis course at Aarhus University.

I have not yet implemented a main function, so for now the code can only be run in an interpreter, e.g. ghci.
Utils.hs contains functions that can handle IO and combines the rest of the modules.

Example session (the numbers in {} are UIDs for the related AST nodes):

> $ ghci Utils.hs
> *Utils> printTypesFromFile "programs/ass1a.tip"
>
> (l{2} == null{7}){6}] = int
> [i{12}] = int
> [1{14}] = int
> [(i{12} + 1{14}){13}] = int
> ...
