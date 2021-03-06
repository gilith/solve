The solve package
=================

The [solve package][] is a [Haskell][] library for solving and
analyzing finite two-player games (e.g., [Fox & Hounds][Fox and
Hounds]).

This software is released under the [MIT License][].

Install
-------

Installing the solve package requires [cabal][]:

    git clone https://github.com/gilith/solve.git
    cd solve
    cabal install --enable-tests

Test
----

Use [cabal][] to run the test suite:

    cabal test

Run
----

The solve package contains an executable called solve, which is run as follows:

    Usage: solve GAME
    where GAME is one of the following:
      NC : Noughts & Crosses
      FH : Fox & Hounds
      QP : Queen & Pawns

Profile
-------

Each game is implemented in a module `src/Solve/GAME.hs` and some have
a size parameter near the top of the file. The performance results in
the `doc` directory were generated by setting this parameter to `SIZE`
and executing the following:

    cabal install --enable-tests
    cabal test
    unbuffer time -v dist/build/solve/solve GAME | tee doc/GAME-SIZE.txt

[cabal]: https://www.haskell.org/cabal/ "Cabal"
[Fox and Hounds]: https://gilith.wordpress.com/2018/11/26/fox-hounds/ "Fox & Hounds"
[Haskell]: https://www.haskell.org/ "Haskell"
[solve package]: https://hackage.haskell.org/package/solve "solve package"
[MIT License]: https://github.com/gilith/solve/blob/master/LICENSE "MIT License"
