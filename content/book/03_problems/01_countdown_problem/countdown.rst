Countdown game
==============

:title: Countdown game
:date: 2023-06-28 06:00
:category: Problems
:status: draft
:summary: Impements a simple number game in Haskell, based off Graham Hutton's Haskell course. 

Overview
********

The countdown problem is a quiz program, similar to the game 24.

The problem is to take a list of numbers and operations, and your goal is to construct a target number.
For example

  | Given: 1, 3, 7, 10, 25, 30
  | Use the operations ``*, +, - , /`` and each number at most once to get to 765

One solution is

.. code:: bash

    765 = 25*30 + 10 + 7 - 3 + 1

which uses every number exactly once.

Thinking about this, this is a tree algo (either BFS or DFS), where the nodes are numbers 
and the links contain an operation. 

Unlike traditional DFS or BFS problems, the graph is not given to us ahead of time.

Other rules:

- At any step in a computation, you must have a positive natural number.
https://www.youtube.com/watch?v=CiXDS3bBBUo&list=PLF1Z-APd9zK7usPMx3LGMZEHrECUGodd3&index=13



