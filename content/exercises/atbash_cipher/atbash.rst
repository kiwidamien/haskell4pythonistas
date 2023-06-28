atBash Cipher
=============

:title: Simple Cipher
:date: 2023-06-28 06:00
:category: Exercise 
:status: published
:summary: todo 

This is a simple substitution cipher (again on `Exercism <https://exercism.org/tracks/haskell/exercises/atbash-cipher/edit>`_) where 
the *core* part of the algorithm is to swap letters with the corresponding reversed letter (e.g. 'a' goes to 'z', 'b' goes to 'y', etc).

There are some additional steps as well:

  * All letters should be lowercased
  * Only letters and  numbers are transmitted through; spaces, punctuation, etc should be stripped
  * The encoded string should be in blocks of 5 (separated by spaces)

For example, ``encode("A aaA AA1 1AA B")`` should be ``zzzzz z11zz b``.

This problem is interesting because it is very simple in Python. There is a Haskell implementation that is not too difficult, but also not very Haskell-y (what I did to start with)
and other solutions that are good for highlighting the differences.

Python version
**************

.. include:: atbash.py
    :code: python


Haskell versions
****************

.. include:: atbash1.hs
    :code: haskell
