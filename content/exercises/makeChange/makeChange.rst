Making Change
=============

:title: Making Change
:date: 2023-07-12
:category: Exercises
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

There are a few ways of addressing this problem. 

- **Use BFS instead of DFS**
  
  BFS excels at shortest path problems, as you are expanding each solution one step at a time. As soone as you find a solution, you can stop, as the first solution found will be (one of) the smallest. Generally BFS is a little more complicated to code, and tends to take more memory as you are storing the state of several branches at once.

  DFS is a little easier to write (at least the recursive version), and excels where you are looking for a solution (e.g. maze solvers).

- **Memoize**

  The problem is spending a lot of time calculating steps it has already calculated. In python, we could get significant gains by using an ``@lru_cache``.

- **Additional information**

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

Despite knowing how to solve the problem, doing it in Haskell was a struggle for me. Here was my first (non-fruitful) attempt. Let's make the final solution throw a "Nothing" if the problem cannot be solved. Our signature is

.. code:: haskell

   makeChange:: Integer -> [Integer] -> Maybe [Integer]
   makeChange amount coins = ....

Let's consider this as a recursive function, using the standard tricks. We will pretend that we have a function, ``makeChange``, that can return a smallest list of coins to make ``amount`` if there is one, or ``Nothing`` otherwise, for all amounts less than the current amount.

For the recursive step, if we want to find ``makeChange amount coins`` we have to

1. Calculate ``makeChange (amount - c) coins`` for each ``c`` in coins
2. Discard the ``Nothing`` values
3. Choose the shortest remaining list

Step 1 is doable because we assumed that ``makeCoins`` works on any amounts smaller than the current amount. Then we just need the base cases:

.. code:: haskell

   makeChange 0 _ = Just []
   makeChange _ [] = Nothing
   makeChange amount coins 
       | amount < 0 = Nothing
       | amount `elem` coins = Just [amount]

Okay, so the last part is the recursive step. The first part, trying 1 coin at a time, seems like a natural fit for a list comprehension:

.. code:: haskell

   possiblePathsIncludingNothings = [makeChange (amount-c) coins | c<-coins]

Here ``possiblePathsIncludingNothings`` is of type ``[Maybe [Integer]]``. We need a way of finding the shotest list, discarding the ``Nothings``. This seems like an accumulator:

.. code:: haskell

   shortestListFinderHelper :: Maybe [Integer] -> [Maybe[Integer]] -> Maybe[Integer]
   shortestListFinderHelper shortest_so_far [] = shortest_so_far
   shortestListFinderHelper Nothing (try:try_s) = shortestListFinderHelper try try_s
   shortestListFinderHelper try (Nothing:try_s) = shortestListFinderHelper try try_s
   shortestListFinderHelper (Just t) ((Just c):try_s) = if (len t) < (len c) then (Just t) else (Just c)

This is really just a reduction in disguise. What we really have is a way of taking two ``Maybe [Integer]`` values and finding the smallest of them, and using this as the reduction step:

.. code:: haskell
hj
   smallestMaybeList :: Maybe [Integer] -> Maybe[Integer] -> Maybe[Integer]
   smallestMaybeList (Just a) Nothing = (Just a)
   smallestMaybeList Nothing (Just b) = (Just b)
   smallestMaybeList Nothing Nothing = Nothing
   smallestMaybeList (Just a) (Just b) = if (len a) < (len b) then a else b


Putting it altogether we get the following program

.. include:: makeChange1.hs
   :code: haskell

Slowness: Memoization
---------------------

Both the Python and Haskell versions are correct, but they are both painfully slow. The following example takes significant time (a little under a minute on my machine) in both cases:

.. code:: python

   >>> make_change(63, [1, 5, 10, 21, 25])
   # wait 30 seconds ....
   [21, 21, 21]

In Python, a simple way to increase the speed is to put a decorator on the function to cache the results!  Let's make this small change, which also requires us to change our coins to tuples.

.. include:: make_change_w_cache.py
   :code: python

With this small change, solving the same problem as we have above is reduced to 168 :math:`{\mu}` s.

What is the Haskell version of memoization?  `Here is a great video on Haskell memoizatoin <https://www.youtube.com/watch?v=Hyxr7SCXpSQ>`_






Appendix
--------

.. include:: leetcode_dfs.py
   :code: python

.. include:: leetcode_bfs.py
   :code: python

.. include:: leetcode_dp.py
   :code: python

From https://leetcode.com/problems/coin-change/solutions/114993/four-kinds-of-solutions-dp-bfs-dfs-improved-dfs/ where the author claims the following times

============= =========
Solution      Time
============= =========
DFS           325 ms
BFS           796 ms
DP            1574 ms
============= =========

for the inputs ``coins = [26, 12, 75, 53, 7, 9, 25, 3, 96, 44, 39, 79, 20, 61, 57, 95, 89, 10, 62, 73, 94, 59, 52, 87, 40, 78, 28, 37], amount = 12312312``
