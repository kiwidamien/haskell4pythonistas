Alphanumerics
===============

:title: Alphanumerics
:date: 2023-08-14 00:10
:category: Exercises
:status: draft
:summary: Solving Letter to Number subsitution problems

The original version of this problem on exercism had some parsing, which we will
eliminate, at least  to start with, in the interest of focusing on the interesting part

Here we are looking at a puzzle where
- Each letter stands for a _different_ digits 
- Leading digits cannot be zero
- Subsitution makes the equation true

For example

.. code:: bash

        S E N D
      + M O R E 
    -----------
      M O N E Y

would have a solution 

.. code:: bash
      9 5 6 7
    + 1 0 8 5
  -----------
    1 0 6 5 2

i.e. M = 1, O = 0, N = 6, E = 5, Y = 2, R = 8, D = 7, S = 9. Note there are not letters that map to the same number,
and that S and M are both non-zero.

We want a function ``solve(["SEND", "MORE"], "MONEY")`` that would return ``dict(M = 1, O = 0, N = 6, E = 5, Y = 2, R = 8, D = 7, S = 9)``.
If there is no solution, the Python function should return ``None``.

Python solution
---------------

Let's work through the specifics of our example. We know that we need to add D and E first, which will give us the condition

.. code:: python

    D + E = Y mod 10

Once we pick these variables, we move to the next column:

.. code:: python 

    R + N (+ carry) = E mod 10,  carry = 1 if D + E >= 10 else 0

This equation is actually a constraint, as a free choice of R and N may violate it because we determined D and E in the previous step.

Column-wise addition suggests that we look at the terms in reversed. There are three solutions that suggest themselves:

1. Do assignment left to right, and then drop when constraints are violated.
2. Do assignment from left to right, but _solve_ constraints where possible.
3. Build a dag and determine the "free" variables.

The first is the least efficient, but also the quickest. 

Simpification 1: Same length
****************************

Let's assume that all our terms are the same length (e.g. length 4, like ``["SEND","MORE"]``).
The result, like ``"MONEY"`` can be longer.

We will evaluate column at at time. We know we start at the ones column:

.. code:: python

    D + E  = Y mod 10

We can reverse each of the terms and zip them together:

.. code:: python

    eqs = ["SEND", "MORE"]
    result = "MONEY"
    rev = [term[::-1] for term in eqs]  # ["DNES", "EROM"]
    zipped = list(zip(*rev))  #[('D', 'E'), ('N', 'R'), ('E', 'O'), ('S', 'M')]
    ans_rev = result[::-1]    # "YENOM"

In the first step in the evaluation, we would have to pick values for ``"D"`` and ``"E"``, which would determine ``Y``.

.. code:: python

    this_carry = 0
    for d in range(0, 9):
        for e in range(0, 9):
            if d == e: continue  # must be unique!
            var['d'] = d 
            var['e'] = e 
            # Because D + E = Y mod 10
            var['y'] = (var['d'] + var['e'] + this_carry) % 10
            this_carry = (var['d'] + var['e'] + this_carry) // 10
            # check for uniqueness of y and carry on.
            ...

So this will essentially be DFS with backtracking.

Procedure:
* We attempt to evaluate the column.
* If we do not have a variable value yet, we subsitution in a guess.
* If 
