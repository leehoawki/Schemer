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
2. Standard library provided.
3. IO functions provided.

## Samples

    ./Schemer
    Scheme> (define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))
    (lambda ("x") ...)
    Scheme> (factorial 10)
    3628800
    Scheme> (load "stdlib.scm")
    (lambda ("pred" . lst) ...)
    Scheme> (map (curry + 2) '(1 2 3 4))
    (3 4 5 6)
    Scheme> (filter even? '(1 2 3 4))
    (2 4)