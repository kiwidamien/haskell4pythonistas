Sum of Multiples: Getting experience with Lists as Monads
=========================================================

:title: SumOfMultiples
:date: 2023-06-27 17:00
:category: Exercise
:status: published
:summary: Exercise demonstrating the Monad properties of Lists in Haskell via a contrived programming task.


Here is a pretty artifical problem from Exercism called `"Sum of Multiplies" <https://exercism.org/tracks/haskell/exercises/sum-of-multiples>`_.
It is one of those contrived exercises designed to show that you can carry out a simple task. 
For us, it will be useful to write it out in Python, and use it as an example of how to get more comfortable with "a list as a Monad".


The description of the problem is

    You work for a company that makes an online, fantasy-survival game.

    When a player finishes a level, they are awarded energy points. The amount of energy awarded depends on which magical items the player found while exploring that level.
    
    The rules for determining points are

    1. For each item, take its base value and get all multiples less than the current level.
    2. Add all the unique multiples together

So as an example, if we are on level 15 and have items with levels 3 and 4, then 

.. code::

    multiples of 3 less than 15: 3, 6, 9, 12
    multiples of 4 less than 15: 4, 8, 12
    sum of multiples = sum({3, 4, 6, 8, 9, 12}) = 42

where the second 12 is removed as a duplicate.


Python version
**************

A working implmentation in Python is not too bad


.. include:: sumOfMultiples.py
    :code: python


We can call ``sumOfMultiples([3, 4], 15)`` and verify that we get ``42``.

One critique I would have is the way the factors are collected. The idea of "summing list of lists, starting with the empty list" is my go to way of flattening a list in Python. It is a cool little one-liner, but could be confusing in this case because we are using `sum` again in the problem to sum numbers.

What the factors line is really doing is a reduce:

.. code:: python

    import functools

    # replace factors = sum(....) above with this
    factors = functools.reduce(lambda a, b: a+b, [multiple(base, upper=level) for base in items], [])

You could also argue that the ``multiples`` function should return a set of integers and not a list. 
In this case, that is fair, but I would be writing this with the idea that maybe this function can be reused somewhere else. 
It is easier to give the list of multiples ordered (because I generated them that way) and if the caller wants 
them unordered to make that the caller's responsibility.

Haskell versions
****************

One of the things that I find difficult about Haskell is relearning all the standard locations for functionality. Here we can use

.. code:: haskell

    import qualified Data.Set as S  -- need qualification so Set map doesn't interfere with List map
    -- gives us S.fromList and S.toList


First version
-------------

Here I started with the multiples function, and got hooked on the idea of a clever infinite generator:

.. code:: haskell
        
    multiples :: Integer -> [Integer]
    multiples n = m
        where m = n : zipWith (+) (m) (repeat n)

This works through lasy evaluation. You want the first element? It's just ``n``!

How about the first two elements? Well the first one is ``n``. 
The second one we need the first element from ``zipWith (+) ([n, ?,?,?, ....]) ([n, n, n, ....]) = [n+n,?+n,...]``. 
Since we only need the first element, we don't have to worry about those question marks, so we get ``[n, 2*n]``.

The first three elements? We know that ``m`` has ``[n, 2*n]`` as it's first two elements. So the zipWith is actually

.. code:: haskell

    m evals as n : zipWith (+) ([n, 2*n, ...]) ([n,n,n,n]) -- expanding out the first two entries in m
            = n: (n+n) : (2*n+n) : .....  -- eval the zipWith
            = n: 2*n : 3*n : ....         -- if we are only taking the first three, lazy eval would stop here


This is a cool Haskell-ish function, but we have put responsibility on the caller to know it generates an infinite sequence. Whenever we call it, we have to make sure that we don't evaluate the whole thing.

Mirroring the Python solution to get the multiples from the list of items:

.. code:: haskell

    multiplesUpTo :: [Integer] -> Integer -> [Integer]
    multiplesUpTo items limit = concat $ map (\n -> takeWhile (< limit) $ multiples n) items


We can dedupe:

.. code:: haskell

    uniqueMultiplesLessThen :: [Integer] -> Integer -> [Integer]
    uniqueMultiplesLessThen items limit = S.toList $ S.fromList $ multiplesUpTo items limit


We can then sum and we are done!

Result
~~~~~~

.. include:: sumOfMultiples1.hs
    :code: haskell 


There a quite a few problems with the code above! Let's go through them:

1. A item ``[0]`` will cause the ``takeWhile`` to run forever! 
2. The ``concat`` and ``map`` next to each other is the sign of a beginning Haskeller! 
   The ``map`` is transforming a my list of integers into a list of lists, and ``concat`` is flattening it. 
   The idea of "map and flatten" are two of the monoid operations, so I should be able to generalize this code.

Second version: Eliminate the infinite loop
-------------------------------------------

In this case, I was just trying to be too clever! There is a one-liner that bounds the result:

.. code:: haskell

    multiplesUpTo :: Integer -> Integer -> [Integer]
    multiplesUpTo base limit = [mult*base | mult <- [0..limit], mult*base < limit]

I think if you take the entire list, it will do all ``limit`` evaluations. Even with lazy evaluation, it has to do the evaluation to know that you don't 
pass the condition.

If you do a ``takeWhile (< limit) $ multipleUpTo base limit`` the laziness will prevent the unnecessary computations. 
Here you are using the fact that you know the results are ordered, so you can stop evaluating as soon as you first fail the condition. 
It is a little weird that to get this optimization you need to specify the upper bound in two different places, though.

A slightly nicer way, is something like this

.. code:: haskell

    multiplesUpTo :: Integer -> Integer -> [Integer]
    multiplesUpTo base limit
        | base == 0 = [0]
        | base > limit = [0]
        | otherwise = takeWhile (< limit) m 
        where m = base: zipWith (+) (m) (repeat base)


Result
~~~~~~

.. include:: sumOfMultiples2.hs 
    :code: haskell 


Third version: Get rid of `concat map` by thinking of list as a Monad
---------------------------------------------------------------------

Now we are looking at the line of code

.. code:: haskell

    concat $ map (\n -> multiplesUpTo n limit) factors


Working this through for our list of items [3,4] and limit of 15:

.. code:: haskell

    concat $ map (\n -> multiplesUpTo n 15) [3, 4]
    concat $ [multiplesUpTo 3 15, mutipliesUpTo 4 15]
    concat $ [[0, 3, 6, 9, 12], [0, 4, 8, 12]]
    [0, 3, 6, 9, 12, 0, 4, 8, 12]


We also know that a list is a Monad. In this case we have ``List Integer``. We have four Monad functions, specialized to the list Monad:

1. **Unit**: Given an ``x``, generate a list of ``x``; in this case ``unit x = [x]`` is our unit.
2. **fmap**: Given a function ``g: X -> Y``, use ``fmap`` to transform it into a function from ``[X] -> [Y]``. This is precisely what ``map`` does!
3. **join**: An operation that removes the outer layer of the monad. In this case, we have ``join: [[X]] -> [X]``, which is precisely what ``concat`` does!
4. **bind**: Transforms a function ``f: X -> [Y]`` to a new function ``bind f: [X] -> [Y]``. In Haskell, this is ``>>=``

Note that ``bind`` most closely resembles what we have in this case. 

* Given a single ``item`` we can generate a list of its multiples. So we have ``f x = multiples x 15``, for example, with signature ``f :: Integer -> [Integer]``
* In our solution, we use ``(map f) :: [Integer] -> [[Integer]]``; i.e. we feed ``map f`` our list of items and we get back a nested list.
* In our solution, we then use ``concat`` (i.e. ``join``) to flatten the list, i.e. ``concat $ map f:: [Integer] -> [Integer]``.

Note that ``concat $ map`` is a mapping takes a function ``g::X->Y`` and gives us ``(concat $ map g):: [X] -> [Y]``. One of the monad identities is


``join $ map`` is the same as ``bind`` (recall ``join`` for maps is ``concat`` -- this is the generalized version)


So instead of writing 

.. code:: haskell

    concat $ map (\n -> multiplesUpTo n limit) factors

we can write

.. code:: haskell

    factors >>= (\n -> multiplesUpTo n limit)


.. note::

    This arguably makes the code less clear *to those that are starting in Haskell*. The real benefit is that it takes something familiar to Python programmers (flattening a list, and mapping over values) and helps build our examples of "what is a Monad?" and "what does bind do again?"

We can do one other thing to tidy this function up. It would have been nice if we had defined the arguments ``multiplesUpTo`` in the opposite order: 
``limit`` first, then the number ``s``. Then instead of writing 

.. code:: haskell

    factors >>= (\n -> multiplesUpTo n limit)

we could write

.. code:: haskell

    factors >>= (multiplesUpToWrittenOtherOrder limit)

The only purpose of the lambda function is to flip the order we apply the arguments. We can do this with ``flip``! So a slightly nicer looking version is

.. code:: haskell

    factors >>= ((flip multiplesUpTo) limit)


Result
~~~~~~

.. include:: sumOfMultiples3.hs 
    :code: haskell
