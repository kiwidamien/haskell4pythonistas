Pascal's Triangle
=================

:title: Pascal's triangle and recursion
:date: 2023-07-03
:category: Exercises
:summary: TBW

Pascal's triangle is a classic recurrion problem, defined by the recursion relation

.. code::

   pascal_element(row, 0) = pascal_element(row, row) = 1,  x >= 0
   pascal_element(row, col) = pascal_element(row-1, col-1) + pascal_element(row-1, col)

There is an alternative formulation in terms of factorials, but for now let's pretend this is the only definition that we know about.

Like the Fibbonacci sequence, we see that each recursive call spawns two more calls, leading to exponential growth of runtime in a naive implementation.

Python verison
**************

The python version of this code is pretty simply to implement, and can be gotten on the first try.

.. include:: pascal.py
   :code: python

Stopping the exponential runtime is as simple as putting a ``@lru_cache`` decorator on the function! We have something that is pretty easy to read, and has good runtime performance.

Haskell versions
****************

Let's start with a naive implementation of the recursive call in Haskell, similar to Python:

.. include:: pascal1.hs
   :code: haskell

Unfortunately, I don't know a simple way of implementating a cache, so this function quickly becomes very slow to execute. On a relatively new mmaching, even 25 rows is quite slow!

Next version: recursion by row
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There is a clever way around this: instead of thinking about recurrsion at the *element* level, we can generate the who **row** using recursion instead

The key observations here are if we have a row of the triangle, like ``[1, 4, 6, 4, 1]``, we can get the next row by:

1. Start with ``1``
2. Add the list to itself, shifted one place over:
   .. code:: python

      [1, 4, 6, 4, 1]
      [4, 6, 4, 1,] = [5, 10, 10, 5]  # drop the one
3. End with ``1``

Step 2 in Python would be "zip the list with itself shifted one index, then add", which would be

.. code:: python

   step_2 = [x + y for x, y in zip(prev, prev[1:])]
   # finishe the calculation
   next_row = [1] + step_2 + [1]

The equivalent in Haskell is ``zipWith``:

.. code:: haskell

   step_2 = zipWith (+) prev (tail prev)
   
   -- or, doing the whole step
   nextRow:: [Int] -> [Int]
   nextRow prev = 1: zipWith (+) prev (tail prev): 1

We need some guard clauses (at the moment, there is nothing stopping us passing an empty list, which would create problems for ``tail``). We can also pass in any list, which is not what we would want -- we only want to be able to pass in lists from Pascal's triangle.

We can make this a private function, and implement ``perow`` so it once again takes an Int:

.. code:: haskell

   perow:: Int -> [Int]
   perow n 
     | n < 0 = []
     | n == 0 = [1]
     | otherwise = nextRow $ perow (n-1)
     where nextRow prev = [1] ++ zipWith (+) prev (tail prev) ++ [1] 

If we want to select a particular element (i.e. implement ``pe``) we can do so by generating the row, and then selecting the element using the index.

Here is the program, rewritten to be recursive on rows instead:

.. include:: pascal2.hs
   :code: haskell

Because we only *calculate* the row once, and reuse it in the recursive call, we are at linear time for calculating a row.

We still have an $O(n^2)$ for calculating the rows (because we still have no memoization).


