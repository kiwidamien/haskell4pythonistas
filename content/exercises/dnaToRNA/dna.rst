DNA to RNA
==========

:title: Converting DNA to RNA
:date: 2023-07-08 13:00
:category: Exercise
:summary: Looks at string conversion (similar to ciphers) but on a very limited alphabet (A, C, G, T). Shows how to deal with ``Either`` as an error handling technique.

This problem is pretty simple: Given a string representing DNA nucleotides A, C, G, and T convert into the matching RNA. The conversion table is

+-------------------+---------------------+
| DNA               | RNA                 |
+===================+=====================+
| A                 | U                   |
+-------------------+---------------------+
| C                 | G                   |
+-------------------+---------------------+
| G                 | C                   |
+-------------------+---------------------+
| T                 | A                   |
+-------------------+---------------------+

Python Versions
***************

In Python, we are going to opt for returning an error flag (instead of raising an exception) when there is an
invalid value. This is to create parity with the Haskell approach. So what we want is something like

.. code:: python

   >>> dna.to_rna("GATTACA")
   ("CUAAUGA", True)  # legal DNA string
   >>> dna.to_rna("CATXCAT")
   ("X", False)       # return the first illegal character and tell us the result is invalid


The overall approach is pretty simple:

- Use a dictionary to map from DNA to RNA, or return a letter with an error flag if it isn't one of A, C, G, or T.
- Join the list of transformed letters back as the "translated" string.

In code

.. include:: dna.py
   :code: python

This function doesn't look particularly elegant, but it does the job. The problem is a list comprehension *almost* works here, except when we
have an illegal letter. We could still do something like

.. include:: dna_elegant.py
   :code: python

This looks more functional, but has some disadvantages:

- We have to do the entire conversion, we don't get to exit early on an error.

  If we had a string the was a few million characters long, and the fifth one is an "X", we still need to process all the characters

- We are less flexible to changing requirements:

  A list comprehension is great, readable, and elegant when the requirements are simple. We can add another requirement (e.g. error checking) and we can work around it. But if there was additional logic (e.g. raise an error if you have a run of more than 10 of the same input consecutively) it becomes harder and harder to work with the comprehension.

The loop isn't that much less readable, but it is a little uglier, but allows us to both exit early and is more flexible to changing requirements. I am not so worried about the lack of early exit ("Premature optimization is the root of all evil", after all!) as we should have nicer code to start and change if profiling tells us that it is an issue. The main advantage to me is the flexibility, although you might think YAGNI (You Ain't Gonna Need It) and stick with the first.

Regardless, we can quickly find two ways of coding this problem up in Python.

Haskell Versions
****************

For the haskell version, let's start with the ``Maybe`` type and work our way to getting errors with ``Either``. We start wanting

.. code:: haskell

   toRNA :: String -> Maybe String

Following the python approach, it is pretty straightforward to use a ``Map`` to turn a String into a list of ``Maybe Char``

.. code:: haskell

   import qualified Data.Map as Map

   processString :: String -> [Maybe Char]
   processString s = map (\x -> Map.lookup x translation) s
       where translation = Map.fromList [('A', 'U'), ('C', 'G'), ('G', 'C'), ('T', 'A')]

Here the ``Map.lookup x translation`` returns either ``Just Char`` if ``x`` was a valid key, or ``Nothing`` otherwise.

In Haskell, a list ``[Char]`` is a string, so there is no need for a conversion. Unfortunately, we have a list ``[Maybe Char]`` that we want to convert to a ``Maybe String``. How do we do that?

Let's start with a function ``pureAppendChar`` defined as follows:

.. code:: haskell

   pureAppendChar :: String -> Char -> String
   pureAppendChar str c = str ++ [c]

What I'd like to be able to do is have a version of this that works with ``Maybe``. Let's write one explicitly, although this seems like the sort of work we would get fancy Monad stuff to do:

.. code:: haskell

   appendChar :: Maybe String -> Maybe Char -> Maybe String
   appendChar _ Nothing = Nothing
   appendChar Nothing _ = Nothing
   appendChar (Just str) (Just c) = Just (str ++ [c])

This is redundant as we are providing the logic of our pure function ``pureAppendChar`` but so it can only be used with ``Maybe``. Let's see how we can use this, and then come back and improve the code later.

Once we have ``appendChar`` the rest is somewhat easy. We want to take our list of ``Maybe Char`` and append them one at a time to our string. 
This is basically a reduction:

.. code:: haskell

   reduceList :: [Maybe Char] -> Maybe String
   reduceList cs = go cs (Just [])
       where go [] s = s
             go (c:cs) s = go cs (appendChar s c)

Reductions in Haskell are done with ``foldl`` or ``foldr``, and we can rewrite this as a reduction. Here we are applying the list from the left, so we will use ``foldl`` (or, better, ``foldl'``). I always have trouble remembering the signature for ``foldl``, which is ``(b -> a -> b) -> b -> t a -> b``. Specialized to the current case:

.. code:: haskell

   -- :i foldl is (b -> a -> b) -> b -> t a -> b
   -- We have a = Maybe Char, b = Maybe String, t = List
   -- So we need (Maybe String -> Maybe Char -> Maybe String) -> Maybe String -> [Maybe Char] -> Maybe String
   -- or         (           reduction step                 ) -> (ini String) -> (input to iterate over) -> output
   reduceList :: [Maybe Char] -> Maybe String
   reduceList cs = foldl appendChar (Just "") cs

The comment explaining ``foldl`` is way longer than the actual function. I won't include that comment, as I imagine that ``foldl`` will become something more familiar to me as I do more Haskell problems.

We can put this altogether into our first solution

.. include:: dna1.hs
   :code: haskell

First improvement: make ``appendChar`` better
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's say that we have our ``pureAppendChar`` function from earlier

.. code:: haskell

   pureAppendChar str ch = str ++ [ch]

This contains the "essential logic" of ``appendChar``. How do we get it to play nicely with the ``Maybe`` context? In the type language, we want to transform ``workOnMaybe`` that has the following signature:

.. code:: haskell

   workOnMaybe :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)

i.e. an operator that can take pure functions that take an ``a`` (for us, String) and ``b`` (for us Char) to return a ``c`` (for us, also String) and make the function work in the ``Maybe`` context.

If we had functions ``a -> b``, the answer would be ``fmap``:

.. code:: haskell

   -- Specialized to maybe, works on any functor
   fmap :: (a -> b) -> (Maybe a -> Maybe b)

Abusing language a little, we can say ``fmap`` only transforms functions with one argument, and we need to work with ``pureAppendChar`` which takes two arguments. The reason this is lazy is in Haskell, all functions technically take one argument.

One approach is to try using partial function application to get us something that ``fmap`` can work on. Let's think of ``pureAppendChar`` as

.. code:: haskell

   -- usual thought: "apply a string and char to get a string"
   pureAppendChar :: String -> Char -> String
   -- instead, we can insert parens to show how we are thinging about it
   pureAppendChar :: String -> (Char -> String)

This is something that ``fmap`` can work with! We might think that we can just partially apply our way out of trouble. Unfortunately this is not the case:

.. code:: haskell

   λ> p1 = fmap pureAppendChar (Just "hi")  -- this works
   -- okay, now p1 should take Maybe Char and return Maybe String
   λ> p1 (Just '!')
   -- error1
   λ> :info p1
   p1 :: Maybe (Char -> String)
   -- we need Maybe Char -> Maybe String

Looking back at the previous block where mentally supplied brackets, it is no real surprise (in retrospect) this happened. Our entire function is contained in a Maybe, the Maybe doesn't "distribute" across its arguments.

There is another operator, ``<*>``, called "app" that does distribute arguments.

.. code:: haskell

   (<*>) :: Applicative f => f (a -> b) -> f a -> f b

Since ``Maybe`` is an applicative, we have ``<*>`` which *does* distribute the Maybe across the arguments. Going back to our example:

.. code:: haskell

   λ> p1 = fmap pureAppendChar (Just "hi")
   λ> :info p1
   Maybe (Char -> String)
   λ> p2 = (<*>) p1
   λ> :info p2
   Maybe Char -> Maybe String
   λ> p2 (Just '!')
   Just "hi!"

If we want to recover our ``appendChar :: Maybe String -> Maybe Char -> Maybe String`` we can do the following

.. code:: haskell

   -- the old way
   λ> appendChar (Just "hi") (Just '!')
   Just "hi!"
   λ> (<*>) (fmap pureAppendChar (Just "hi")) (Just '!')
   Just "hi!"

   -- we have an infix version too
   λ> (fmap pureAppendChar (Just "hi")) <*> (Just '!')
   Just "hi!"

   -- fmap also has an infix version, <$>
   λ> (pureAppendChar <$> (Just "hi")) <*> (Just '!')
   Just "hi!"

   -- these operations are associative, so we can remove parens
   λ> pureAppendChar <$> (Just "hi") <*> (Just '!')
   Just "hi!"

   -- we can actually rewrite appendChar this way
   appendChar s c = pureAppendChar <$> s <*> c

In a practical sense (not a purist "haskell functions only have one argument" sense) it is useful to think of ``fmap`` or ``<$>`` getting our function application *into* the context, and ``<*>`` allowing us to stay there.

This gives us our second solution

.. include:: dna2.hs
   :code: haskell

Third version: moving to Either
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To match the Python solution, we need to do better than return ``Nothing`` when we input an invalid string. We need to be told the first invalid character. We will do this using the ``Either`` monad (which is therefore also a functor and an applicative).

We will take the standard convention that the left value is the error, and the right is the value. Let's break off the translation into it's own step

.. code:: haskell

   translate :: Char -> Either String Char
   translate 'A' = Right 'U'
   translate 'C' = Right 'G'
   translate 'G' = Right 'C'
   translate 'T' = Right 'A'
   translate c = Left ("Cannot translate " ++ [c])

We now need to be able to reduce ``[Either String Char] -> [Either String String]``. ``Either`` already has ``fmap`` defined on it:

.. code:: haskell

   instance Functor (Either a) where
       fmap f (Right x) = Right (f x)  -- i.e. right values "commute"
       fmap f (Left x) = Left x  -- i.e. Left or error values short circuit

Keeping our same ``pureAppendChar`` we can keep ``reduceList`` almost completely the same. The things that change are the type signature, and the initial value of the string we are building (going from ``Just ""`` to ``Right ""``)! The entire program is now

.. include:: dna3.hs
   :code: haskell

Fourth version: ErrMsg type alias
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a pretty short refactor, but when looking at ``Either String String`` in particular, it can be hard to read what the intent it. The general format we have is ``Either String A``, where the left ``String`` contains an error message, and ``A`` contains the data.

Let's make a type alias to make this a little clearer

.. include:: dna4.hs
   :code: haskell

Fifth version: Bringing back the dictionary
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The translate function is taking a lot of space, and is still acting like a Python Dictionary / Haskell Map. Let's restore the Haskell Map! I place it outside the function, so I am not recreating the Map on every lookup.

Here we use ``findWithDefault``, which is similar to the Python ``get`` method

.. include:: dna5.hs
   :code: haskell


Summary
*******

Implmenting this in Python was pretty straightforward. It took me a little longer to write this in Haskell, because working with dictionaries / maps is a little tricker.

In Python:

* You have to make sure that the key you are trying to access exists, or you will get a ``KeyError``
* or you can use ``get`` to get a default value.

You can easily check that the value you want is there before access, so you are left with "plain old values" to manipulate.

In Haskell:

* When you access a key, you will get a ``Maybe`` result (even if you are SURE the key is in there, because you just checked).
* You can use ``findWithDefault``, similar to ``get``.

In my Haskell code, the hard part was trying to figure out how to work with the contexts. We approached this by

1. Started with just ``Maybe`` (what we get from the Map ``lookup`` method) and writing a specialized append function for ``Maybe Char`` and ``Matybe String``
2. Used functor ``<$>`` and applicative ``<*>`` to generate a specialized append function from the pure function.
3. Moved from ``Maybe`` to ``Either``, so we could carry error information. We saw the "typeclass"-ness of ``<$>`` and ``<*>`` meant that we didn't have to change the code that used these pieces; the contexts (``Maybe`` and ``Either``) applied the correct semantics for these operations.
4. Made a type class for ``ErrMsg`` to make the intent of the ``Either`` types easier to reason about.
5. Moved back to a dictionary with defaults, instead of a 5 line function, for translation.


