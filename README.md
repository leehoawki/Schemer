# Schemer

A Scheme interpreter in Haskell based on 

[Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)

[渣翻中文版](http://www.jianshu.com/p/b80a06bfd3a7)


## Build

	ghc -package parsec -o Schemer Schemer.hs
    ./Schemer "(+ 1 2)"
    3

## Features
1. REPL supported.
2. Standard Lib provided.
3. TODO