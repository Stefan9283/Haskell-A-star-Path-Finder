
# Hunter Game

Second-year bachelor's degree assignament for Programming Paradigms

A game implementation of the A* path finding algorithm.

## How to use
```
    stack exec ghci --package PSQueue Interactive.hs 
```
From the interpreter then you can do:
```
    *Interactive.hs> interactive $ loadGame "terrains/terrain-1.txt" [goNorth, goSouth] [(0, 1)]
                    --> start the game in terminal using the map terrain-1.txt
                    --> use CTRL + C to quit

    *Interactive.hs> hunt True $ loadGame "terrains/terrain-1.txt" [goNorth, goSouth] [(0, 1)]
                    --> start the game in terminal using the map terrain-1.txt
                    --> press ENTER to move based on the choice made by the A* algorithm
```

## Dependencies
PSQueue ("stack install PSQueue" to install)
