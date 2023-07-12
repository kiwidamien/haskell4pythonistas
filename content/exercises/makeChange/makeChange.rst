Making Change
=============

:title: Making Change
:date: 2023-07-12
:status: draft

Given an amount of money ``target``, and possible coins ``coins``, how can we make ``target`` using the fewest number of coins?

Going through a few examples

.. code:: python

   >>> make_change(target=25, coins=[1, 5, 10, 25, 100])  # the US coin denominations
   [25]  # can make 25c using just one piece
   >>> make_change(target=55, coins=[1, 5, 10, 25, 100])
   [5, 25, 25]
   >>> make_change(target=23, coins=[1, 4, 19, 20])
   [4, 19]  # Note it isn't [1, 1, 1, 20] which requires 4 coins
   >>> make_change(target=12, coins=[1, 2, 10, 11])
   # can return either [1,11] or [2, 10] -- both are valid
   >>> make_change(target=9, coins=[2, 4])
   ValueError of some sort # Cannot make an odd amount from only even coins!

DFS: the strategy
-----------------

The python version is an implementation of depth-first search. We recognize that in order to make change for ``c`` cents, we pay one of the coins ``X``, and then solve the same problem for an amount ``c-X``. The base cases are:

* ``c < 0``: oops, gone too far -- we cannot make change this way.
* ``c == 0``: found a solution, don't need to return any more coins.

The easy mistake to think is that we only have to try the largest coin less than ``c``, and if there is a solution it will be the shortest. Sadly this isn't true, as the example with coins ``[1, 4, 19, 20]`` shows:

.. code:: python

   # NOTE: THIS IS WRONG
   make_change(23, coins=[1, 4, 19,20])
   # can we make change from 20? Try calling 
   = [20] + make_change(3, coins=[1,4,19,20])
   # can we make 3 from remaining coins? Highest coin smaller than 3 is 1
   = [20] + [1] + make_change(2, coins=[1,4,19,20])
   # and so on ...
   = [20] + [1] + [1] + make_change(1, coins=[1,4,19,20])
   # and again
   = [20] + [1] + [1] + [1] + make_change(0, coins=[1,4,19,20])
   # and that's the base case
   = [20, 1, 1, 1]

As we can very by hand ``[19, 4]`` is a shorter solution that makes 23. If we assume make change has the shortest amount to make the sum, what we really need to call for the recursive step is

.. code:: python

   # pseudo-code
   make_change(23, coins) = select_shortest_length([[c] + make_change(23 - c, coins) for c in coins])

We assume that ``make_change(amt, coins)`` returns a shortest length list of coins. We can try each of the coins in turn, getting the shortest list we can make from the left over amount. To ensure that we keep the shortest list constraint, we then take the shortest of these lists.

One downside to this approach is that we have no way of early exiting. For example:

.. code:: python

   make_change(101, coins=[1, 50])
   # this code would have one DFS generate [1, 50, 50] early on, but would also construct
   # [1, 1, ...., 1, 1] as a potential solution before discarding it

The natural way of solving this problem is with BFS, as you can stop as soon as one path yields a solution. DFS excels when finding *a solution* is enough (e.g. maze exploration) as it is often simpler to code and takes less memory.

We are using DFS here simply because it is easier to write the recursive version.

Python version of DFS
---------------------

.. include:: make_change.py
   :code: python

Note that we don't have the easy out of making the recursive part throw an error. 
All that would mean is this particular choice didn't lead to a valid solution.
Instead, we get the ``_make_change`` helper function to return the list of coins it has, as well as a "helper flag" to let the caller know if the list it returns is actually a solution, or merely a collection of dead ends.

Haskell version
---------------


