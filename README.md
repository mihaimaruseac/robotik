Robotik
=======

This repository contains the code for a compiler of Robotik language, one of
the entries for [PLT Games Contest](http://www.pltgames.com/competition/2012/12)

Language Description
--------------------

The language is inspired by the game [Ricochet Robots](http://en.wikipedia.org/wiki/Ricochet_Robot).

Indeed, the active units in the execution of a program in Robotik are some
robots. A program consists of a description of the initial placement of the
robots followed by a list of directives. Each directive is an order given to a
robot to move in one direction and write a value at the stopping point.

Each robot has a modulus. It cannot pass through cells containing another
robots or values which are exact multiples of this modulus. The following
image is an example (for a robot named `R1` with modulus `3` ordered to move
to the right; assume a [closed world](http://en.wikipedia.org/wiki/Closed_world_assumption)).

![img1](http://s19.postimage.org/xp755y7tf/moving.png)

If one robot has no stopper on the direction it is ordered to move, then the
robot keeps his position but writes the value nonetheless.

There is one exception to the above rule. Robots with modulus equal to `0`
cannot be stopped by cells with values. They are special robots and have two
interesting properties. They always push the robot they collide with one cell
forward, if this is possible. After this, the next directive to execute is one
of the previous directives given to the pushed robot. More exactly, if the
directive given to the 0-modulus robot has value `v` then the next directive
to execute is the `v`-th latest directive for the pushed robot, if any. In
case there are no directives for the pushed robot then execution continues
normally. However, if there are directives for it but not enough, execution
continues from the first directive of said robot. See the following image for
an illustration (assume `...` mean more directives and the arrows on the right
show the jumping process).

![](http://s19.postimage.org/pxqf7e3o3/0mod.png)

The above exception was introduced to enrich the language with loops, thus
making it Turing complete (taking together all other features)

The world is an infinite 2D lattice of integer values. Thus, the entire
program is made from **integer values only**. The position of these integers
and their relationships are the ingredients which make Robotik an interesting
language.

Not all sequences of numbers are valid programs. Some values could be reduced
to others using modular arithmetic. But others cannot. There are a few
restrictions:

1. The number of robots must be a positive value `R`.
1. Each modulus should be a nonnegative value.
1. There should be at least `3*R + 2` values in the program. If a directive is
   incomplete it will be padded with `0` values. However, there is no program
   with no directives or with an incomplete robot specification.

Nevertheless, even with these restrictions the language accepts almost all
streams of numbers. If we restrict our input to non-negative numbers and to
streams starting with low values then the ratio between valid inputs and
possible inputs approaches 1. This can be useful for chaining multiple
programs together, for considering the output of one program as being another
program or for stenographic purposes. It is easy to embed a program inside an
image and carry it through unsuspecting eyes. Even a little encryption can
help a long way here :)

The output of such a program is the entire state of the lattice.

Language Implementation
-----------------------

Because we cannot work with infinite lattices the display is limited to the
minimal axis-aligned rectangle enclosing all robots. This is the only
modification done between the theoretical language illustrated above and the
practical implementation given by this repository.

The compiler builds a board from the incoming numbers and moves robots on it
according to the directives. Because only the final board is able to produce
output endless running programs will loop forever without producing anything
usable.

Busy Beaver example
-------------------

In the `srcs` directory, there are two sources for the busy-beaver type of
program. In order to implement looping we have to use 0-modulus robots an the
reason for starting with this section is to give examples of their role.

First, we have `srcs/busyBeaver` which is a program which will print 4 values
of `1` before stopping. `R1` will constantly push `R2` to the left while `R2`
will always write a `1` in the current cell.

![](http://s19.postimage.org/qbrr6znrn/beaver.png)

Secondly, there is `srcs/endlessOnes` which will try to create the state where
all values on `Ox` axis are `1`. However, since that state is unreachable, the
program will loop forever.

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

As a more interesting exercise, try to rewrite the last example using 0-modulus
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
