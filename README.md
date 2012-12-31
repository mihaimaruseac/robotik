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

Busy Beaver example
-------------------

In the `srcs` directory, there are two sources for the busy-beaver type of
program.

First, we have `srcs/busyBeaver` which is a program which will print 4 values
of `1` before stopping.

TODO description

Secondly, there is `srcs/endlessOnes` which will try to create the state where
all values on `Ox` axis are `1`. However, since that state is unreachable, the
program will loop forever.

TODO description

Hello World examples
--------------------

The classical program which prints "Hello World" is illustrated by 4 examples.
Of course, there could be more. Each of the 4 examples demonstrates one of the
features of the Robotik language.

To start with, we have `srcs/hello1`. The initial situation can be illustrated
by the following picture.

TODO picture

The only moving robot is `R2`. It sweeps the board between `R1` and `R0`
writing a value at each stop, according to the ASCII value of the letter which
needs to be written there. Because it has modulus `1` it will stop before each
previously written value, thus reducing the width of the sweep. The following
image is an illustration of this program in action.

TODO picture

Turning our attention to `srcs/hello2` we see that it has the same length and
overall structure as `srcs/hello1` with two differences. The numbers are no
longer on a single line but you already know that this is no significance to
the language. What matters is that the numbers in the directives part are
increasing. The program does the same thing as `srcs/hello1` because each
number is translated in the appropriate range during the building stage of the
compiler using modular arithmetic.

The robots in `srcs/hello3` are still printing "Hello World" but using a
different strategy: the robot `R2` first tries to move on the `Oy` axis. Since
there is no blocking point on that axis the robot keeps its position and
writes its value at that point. The `R2` moves to `R1` and backwards. Since
its modulus is `1` it will stop at the next position. The following image
gives the first step of the execution.

TODO picture

Lastly, `srcs/hello4` uses 4 robots but only `R0` and `R1` do actual work. The
following image illustrates the execution of this program.

TODO picture

As an exercise, try to deduce the translation from the code in `srcs/hello4`
to the above image.

All of the example have used two robots which are only sitting idle. Their
purpose is to extend the range of the output values such that the entire
message is printed. Also, they have a role in limiting the initial walk of the
active robots.

As a more interesting exercise, try to rewrite this example using 0-modulus
robots and looping. It is going to be a fascinating experience.

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

However, there are some programs which will give the same output, even if
using randomness. See `srcs/random` example.

Open questions
--------------

1. What is the minimum description of a program which outputs "Hello World"?
1. What is the minimum description of a busy-beaver program?
1. What is the minimum description of a program exhibiting the greatest
   variety due to randomness?
1. Can someone implement Game of Life in Robotik?
