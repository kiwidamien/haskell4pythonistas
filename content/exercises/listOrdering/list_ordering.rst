Partial Ordering on Lists
=========================

:title: Partial Ordering on Lists 
:date: 2023-06-29 17:00
:category: Exercises
:status: published
:summary:  Implementating a partial ordering on lists.

Let's look at a (partial) ordering of lists, where we want to say one ``list1`` is less than or equal to ``list2`` if ``list1`` is a sublist of ``list2``. Unlike subsets, the order matters! If ``list1`` is less than or equal to ``list2``, we can also say ``list2`` is greater than or equal to ``list1``. 

It is also possible that two lists cannot be compared, such as ``[1,3]`` and ``[6, 8, 9]``. In this case, neither list is a sublist of the other, so we will not define an ordering. This is what makes our list comparison a _partial_ ordering -- we are not committing to being able to order any two arbitary lists.

We would like to make a function that takes two lists, ``list1`` and ``list2`` and returns

* ``Ordering.LT`` (less than) if ``list1`` is a sublist of ``list2``, and not equal.
* ``Ordering.EQ`` (equals) if ``list1 == list2``
* ``Ordering.GT`` (greater than) if ``list2`` is a sublist of ``list1``, and not equal.
* ``Ordering.NONE`` if there is no comparison between ``list1`` and ``list2``.


Examples
~~~~~~~~

- ``[2, 4, 6]`` is ``Ordering.LT`` ``[0, 2, 4, 6, 8, 10]``
- ``[]`` is ``Ordering.LT`` ``[8, 13, 8]``
- ``['California', 'Oregon', 'Washington', 'Arizona']`` is ``Ordering.GT`` ``['Oregon', 'Washington']``
- ``['Oregon', 'Washington']`` is ``Ordering.LT`` ``['California', 'Oregon', 'Washington', 'Arizona']``
- ``['Oregon', 'Washington']`` is ``Ordering.NONE`` ``['Washington', 'Oregon']``

It is a lot like comparison between sets, except that order (and repetitions) matter.


Python Version
~~~~~~~~~~~~~~

We know how to check if two lists are equal to each other. What remains is to check if one list is a sublist of the other (we can use this to get both less than and greater than, just by swapping the order of the lists).


.. include:: list_ordering.py
   :code: python

The logic here is pretty straightforward. If we use ``list_starts_with`` we can trace through the list, checking each of the locations where candidate list's first element matches a containing list element.

Technically, this check isn't required -- it would be caught by the element-wise check in ``list_starts_with`` -- but it seems wasteful to call this function on every element if it just fails on the first step.


Haskell Versions
~~~~~~~~~~~~~~~~

Here is my first Haskell version of the program, translated pretty directly from the Python version:

Direct translation
******************
.. include:: list_ordering1.hs
   :code: haskell

I don't really like that ``listStartsWith`` _and_ ``isSublist`` both have the "empty list" checks.

They are needed on ``isSublist`` because otherwise we run into an issue with the ``tail y`` part in the recursion.
We keep stripping off the head of the "containing" list, but eventually we reach the tail, and then if we did not guard
against ``isSublist _ []`` in the pattern above, we would have an error.

The *pattern* is not strictly necessary, we could have other checks like is one

.. code:: haskell

   -- replacing isSublist
   isSublist :: (Eq a) => [a] -> [a] -> Bool
   isSublist x y = (listStartsWith x y) || ( ((length x) >= (length y)) && (isSublist x (tail y)) )

which reduces the number of line, but it still feels like we are repeating some of the logic.

Next version
************

While I wish I could claim I thought of this, someone else had an amazing solution. In ``Data.List`` there is a function ``tails`` that 
gives a list containing the subsequent tails of your list. 

.. code:: haskell

   -- tails is in Data.List, tails:: [a] -> [[a]]
   tails [1, 2, 3]
   -- returns [[1, 2, 3], [2, 3], [3], []]

We will show how to implement ``tails`` ourselves, but if we just imported it, how would we use it?

All the function ``isSublist`` does is see if the list is equal to the first N elements of at least one of the ``tails`` of the other list.
So we can use Haskell's equivalent of Python's ``in``, called ``elem``, to see if we have a match in the list, after cutting to the first N elements.

.. code:: haskell

   import Data.List (tails)

   isSublist:: (Eq a) => [a] -> [a] -> Bool
   isSublist x y = x `elem` (tails y)

Now this simplifies the rest of the program, and we don't even need ``listStartsWith`` anymore. Our entire program is

.. include:: list_ordering2.hs
   :code: haskell

We could even eliminate ``isSublist`` as a function, and replace our code with

.. include:: list_ordering3.hs
   :code: haskell

Did we just cheat by importing?
*******************************

In any language, we can import a package that has someone else doing the hard work for us. How difficult would it be for us to implement ``tails`` on our own?

Surprisingly easy! Here is an implementation:

.. code:: haskell

   ourTails :: [a] -> [[a]]
   ourTails [] = [[]]
   ourTails xs = xs : (ourTails $ tail xs)  -- or you might perfer ourTails x:xs = x : (ourTails xs) 


