Robotik
=======

This repository contains the code for a compiler of Robotik language, one of
the entries for [PLT Games Contest](http://www.pltgames.com/competition/2012/12)

Language Description
--------------------

TODO

Language Implementation
-----------------------

TODO

Hello World examples
--------------------

TODO

Busy Beaver example
-------------------

TODO

Turing completeness
------------------

The theoretical language can be easily translated into a classical Turing
machine. The implementation has the same propriety but this is not as easily
seen.

Anyway, proofs of the above claims are left as an exercise to the reader. As a
hint, first you need to reduce this problem to a 2D Turing machine with
multiple heads and proceed from there.

Randomness
----------

In order to exhibit randomness, the only valid solution is to place two or
more robots at the same location. The builder of the internal structure used
by the compiler will take care to randomly move robots around until no
overlaps are found.

Of course, this random moving will create program species which will exhibit
divergent behaviours. But this is intended :)

Open questions
--------------

1. What is the minimum description of a program which outputs "Hello World"?
1. What is the minimum description of a busy-beaver program?
1. What is the minimum description of a program exhibiting the greatest
   variety due to randomness?
1. Can someone implement Game of Life in Robotik?
