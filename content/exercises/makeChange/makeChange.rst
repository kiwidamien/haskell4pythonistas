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


