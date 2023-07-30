Minesweeper
===========

:title: Making Change
:date: 2023-07-29
:category: Exercises
:status: published

Here we are trying to implement one piece of Minesweeper, which is to be able to render the board.
The biggest part that we need to implement is determining the number of mines adjacent to each cell (including diagonals).
This number is between 0 and 8 (because we don't check the cell itself, just its eight neighbours).

The actual function we are asked to implement is a function that will render the board, meaning it will take a list of strings with spaces and `*`, representing
empty cells and mines respectively. The function annotate will produce a list of strings, where the character in row ``r`` and column ``c`` is:

- ``*`` if it is the location of a mine 
- :code:` ` (i.e. space) if it is not a mine, nor adjacent to any mine (i.e. our helper function returns 0)
- A digit ``1`` - ``8`` representing the number of adjacent mines

For example

.. code:: python 

   grid = ["      ",
           " *  * ",
           " *    ",
           "      ",
           "     *"]
   annotate(grid)
   # Should be 
   #      ["111111",
   #       "2*21*1",
   #       "2*2111",
   #       "111 11",
   #       "    1*"]  



Python version of ``annotate``
******************************

The implmentation here is pretty straightforward. One of the things that makes it a little tricky 
is that we are not just writing out the number of adjacent mines, as there are special rules if the
location is itself a mine, or if the location is not adjacent to any mines. The other issue is the
potentially messy checking of boundary conditions.

The boundary conditions are fixed by indexing the locations we want to check, and catching an ``IndexError``
if one is raised. 

The other simplification is that we can write our function that counts the number of
mines to count those at the current location _and_ the adjacent locations. If we are at the location of
a mine, this will make the number higher by 1 than if we wanted to just get the number of strictly adjacent mines, 
but if we are on a mine location we don't use the number (we use the token ``*`` instead).
We are free to implment our helper function as either a function that counts all mines in the neighborhood of the function,
or one that counts all mines in the neighborhood excluding itself.

.. include:: minesweeper.py
   :code: python


Haskell version
***************

First attempt
-------------

Let's start with a function that, given an array and a location, will extract out the
set of neighbors (generally a 3x3, but differs on the edges). We just want to produce them as a flat list.

We do this with a list comprehension:

.. code:: haskell

   neighborhood :: Int -> Int -> [[a]] -> [a]
   neighborhood _ _ [] = []
   neighborhood row col grid = [grid !! r !! c |r<-[(row-1)..(row+1)], c<-[(col-1)..(col+1)], (r,c)/=(row,col), r>=0, r<(length grid), c>=0, c<length(grid!!0)]

The guard against ``[]`` is needed because we get the number of columns by looking at the first row,
so we need to implement what happens if there is no first row.

This was the hardest function to write. A big part of it was resistance on my part of indexing
directly into the lists (twice!). I spent a long time thinking about how to zip the current row with
the previous and next rows to get the sum of the number of mines in adjacent rows.

Once we have the neighborhood, we can define the equivalent of ``count_stars`` in the Python version.
We just need to be able to count the number of times the character ``*`` appears.

.. code:: haskell

   -- Same job as count_stars in Python code
   scoreCell :: Int -> Int -> [[Char]] -> Int
   scoreCell row col grid = countOcc '*' $ neighborhood row col grid
       where countOcc char [] = 0
             countOcc char (x:xs) = (if x==char then 1 else 0) + (countOcc char xs)

The helper ``countOcc`` looks like a pretty generic Haskell function, taking a list
and counting the number of times a value appears.
Searching on `Hoogle <https://hackage.haskell.org/package/MissingH-1.6.0.0/docs/src/Data.List.Utils.html#countElem>`_
I found ``countElem`` with a much nicer implementation than mine.

.. code:: haskell

   -- in Data.List.Utils
   countElem :: Eq a => a -> [a] -> Int
   countElem value = length . filter (value==)

We can simplify ``scoreCell`` to

.. code:: haskell

   -- Same job as count_stars in Python code
   scoreCell :: Int -> Int -> [[Char]] -> Int
   scoreCell row col grid = length $ filter ('*'==) $ neighborhood row col grid

The rest of the code is taking these functions and "mapping" them over the original board
to get the number of mines in the neighborhood of each cell, and then finally converting these
neighborhood numbers to characters (including the special rules for locations of mines, and places with no mines).

The complete program is here

.. include:: minesweeper1.hs 
   :code: haskell 

Second attempt
--------------

There are a few things I don't like about this first attempt

- We have a lot of ``Int`` types floating around. Sometimes the are positions (rows, columns) and sometimes they are counts. It is hard to tell what argument goes where from the function signature.
- The indexing into the list in ``neighborhood`` feels inelegant
- The process of getting of zipping two 2-D lists seems really cumbersome.

The second point requires changing the entire algorithmic approach. Let's start by looking at the other two points.

Reducing type ambiguity
~~~~~~~~~~~~~~~~~~~~~~~

The first point can be solved with a new typeclass, ``Point``.

.. code:: haskell

   type Point = (Int, Int)

   neighborhood :: Point -> [[a]] -> [a]
   neighborhood _ [] = []
   neighborhood (row, col) grid = [grid !! r !! c | r<-[(row-1)..(row+1)], c<-[(col-1)..(col+1)], (r,c)/=(row,col), r>=0, r<(length grid), c>=0, c<length(grid!!0)]

   scoreCell :: Point -> [[Char]] -> Int
   scoreCell pt grid = length $ filter ('*'==) $ neighborhood pt grid

   scoreGrid :: [[Char]] -> [[Int]]
   scoreGrid [] = []
   scoreGrid grid = [[scoreCell (row, col) grid | col<-[0..(numCols-1)]] | row <- [0..(numRows-1)]]
      where numCols = length $ head grid 
            numRows = length grid 

The double zipper
~~~~~~~~~~~~~~~~~

The line of code with two ``zip`` calls and two ``map`` calls is a little unweildy.

.. code:: haskell

   numsToString :: [[Int]] -> [String] -> [String]
   numsToString nums origMap = map (\(lineNums, lineChars) -> map (\(n,s)-> resultToChar n s) $ zip lineNums lineChars) $ zip nums origMap

To understand what it is trying to do, recall it isn't quite enough to know the number of cells in the neighborhood that are mines. We also have to know which of the original
locations were mines.

Ultimately, what I want is a data structure like 

.. code:: haskell

   -- representing the board
   -- ["  *",
   --  "   "]
   zipped = [
      [(0,' '), (1,' '), (1, '*')],
      [(0,' '), (1,' '), (1, ' ')]
   ]

Going tuple by tuple, if the second entry is ``*`` this is what we should render.
If it is space, we should use the first element to determine what character to make (either a space or a digit).

Let's assume we have ``zipped`` above, then our code would be 

.. code:: haskell

   render:: [[(Int, Char)]] -> [[Char]]
   render zipped = map (\line -> map (resultToCharTuple) line) zipped
       where resultToCharTuple (_, '*') = '*'
             resultToCharTuple (0, _) = ' '
             resultToCharTuple (n, _) = chr(ord('0') + n)

Constructing the zipped data structure requires a couple of steps. Using ``zip`` zips the lines together:

.. code:: haskell

   example = ["  *", "   "]
   scores = [[0,1,1],[0,1,1]]

   zip scores example
   -- [([0, 1, 1], "  *"), ([0, 1, 1], "   ")]

To get the data structure I want, I'd have to map over this, and then zip the first and second entries of each tuple.

.. code:: haskell

   map (\(one, two) -> zip one two) $ zip scores example
   -- [[(0,' '),(1,' '),(1,'*')],[(0,' '),(1,' '),(1,' ')]], i.e. zipped!


Instead of doing this in two steps (creating the zipped structure and then reducing), we can also use the ``zipWith`` function.
The purpose of zipWith is to take a function ``a -> b -> c`` and use it to "reduce" the list while zipping. For example

.. code:: haskell

   zipWith (+) [1, 2, 3] [10, 20, 30] = [1+10, 2+20, 3+30] = [11, 22, 33]

Let's start with a function that takes a row of scores, and a row of the original board:

.. code:: haskell

   innerFunc [0, 1, 1] "  *" = ??????? = " 1*"

If we are using ``zipWith``, we need a function that takes an ``Int`` and a ``Char`` and gives a ``Char``.
If we remove the tuple from ``resultToChar``, we already have our candidate!

.. code:: haskell

   innerFunc [0, 1, 1] "  *" = zipWith resultToChar [0, 1, 1] "  *"
   -- changing the helper function resultToChar to take two args, instead of a tuple
   -- i.e. 
   innerFunc scores line = zipWith resultToChar scores line

We can change ``numToStrings`` 

.. code:: haskell

   numsToString :: [[Int]] -> [String] -> [String]
   numsToString nums origMap = map (\ (lineNums, lineChars) -> zipWith resultToChar lineNums lineChars) $ zip nums origMap

Notice that the reason we are zipping ``nums`` and ``origMap`` together is so we can map over the lists together.
We can eliminate the ``map (f) $ zip a b`` pattern with ``zipWith fmod a b`` to get a much simpler function, where ``fmod`` is a version 
of ``f`` that takes two arguments (rather than one tuple with two entries)

.. code:: haskell

   numsToString :: [[Int]] -> [String] -> [String]
   numsToString nums origMap = zipWith (\ lineNums lineChars -> zipWith resultToChar lineNums lineChars) nums origMap

We can even eliminate some names. In the lambda function on the inside, we are simply applying these arguments in the same order.
We can eliminate the lambda and reduce to 

.. code:: haskell

   numsToString :: [[Int]] -> [String] -> [String]
   numsToString num origMap = zipWith (zipWith resultToChar) nums origMap

If we want to, we can even eliminate all the explicit arguments. I like having them there, as it gives me the option to use meaningful names,
but we can reduce this to

.. code:: haskell

   numsToString :: [[Int]] -> [[String]] -> [[String]]
   numsToString = zipWith (zipWith resultToChar)

Final version two
~~~~~~~~~~~~~~~~~

Here is our second attempt at this problem in Haskell.

.. include:: minesweeper2.hs
   :code: haskell


Commentary on Haskell versions
------------------------------

There were a few generalizable learnings here. 
The biggest one was on the use of ``zipWith`` and when to use it.

I have used it with 1-D lists before, but my natural inclination when looking at a pair of 2-D lists that I 
wanted to zip together element-wise and then reduce actual made the problem harder. 
The simpler version of the problem was to use ``zipWith`` twice.
It also meant that I could write my functions as functions that took two arguments for the reduction step, instead
of artificially placing the arguments in a tuple.

If we do want to create an elementwise zip function, here is one way of doing it:

.. code:: haskell

   elementWiseZip :: [[a]] -> [[b]] -> [[(a,b)]]
   elementWiseZip = zipWith (zipWith (\ one two -> (one, two)))

but often times we can skip creating this function altogether and replace the inner lambda with a reducing function.

The second thing I noticed was that Python's ``IndexError`` catching made dealing with the boundary conditions relatively straightforward.
The Haskell ``neighborhood`` function has a lot of index checking built in:

.. code:: haskell
   
   neighborhood (row, col) grid = [grid !! r !! c | r<-[(row-1)..(row+1)], c<-[(col-1)..(col+1)], (r,c)/=(row,col), r>=0, r<(length grid), c>=0, c<length(grid!!0)]

We could separate this out into its own function, but it is something that was significantly easier in Python.