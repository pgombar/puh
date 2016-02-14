University of Zagreb
Faculty of Electrical Engineering and Computing

PROGRAMMING IN HASKELL

Academic Year 2014/2015

LECTURE 12: Monads 1

v1.0

(c) 2014 Jan Šnajder

==============================================================================

> import Data.List
> import Data.Char
> import Control.Monad
> import Data.Maybe

== INTRO =====================================================================

Monads are an important concept in Haskell. They abstract various COMPUTATIONAL
MODELS: 

  * computation with state
  * non-deterministic computation
  * computation that can fail, etc. 

The most prominent example of a monad is the IO monad, which we use for IO
operations.

== A MOTIVATING EXAMPLE ======================================================

Let's motivate the use of monads with a simple example. Recall the (recursive)
data structure 'Person':

> data Sex = Male | Female deriving (Show,Read,Eq,Ord)
> data Person = Person {
>   forename :: String,
>   surname  :: String,
>   sex      :: Sex,
>   mother   :: Maybe Person,
>   father   :: Maybe Person,
>   partner  :: Maybe Person,
>   children :: [Person] } deriving (Show,Read,Eq,Ord)

Let's define some values:

> pero  = Person "Pero" "Perić" Male    (Just ana) Nothing Nothing    []
> ana   = Person "Ana"  "Anić"  Female  (Just tea) Nothing Nothing    [pero]
> tea   = Person "Tea"  "Teić"  Female  Nothing    Nothing (Just ivo) [ana]
> ivo   = Person "Ivo"  "Ivić"  Male    Nothing    Nothing (Just tea) [ana]

Now, let's consider two functions:

> grandmothersPartner :: Person -> Maybe Person
> grandmothersPartner p = case mother p of
>   Just m -> case mother m of
>     Just g  -> partner g
>     Nothing -> Nothing
>   Nothing -> Nothing

> partnersForename :: Person -> Maybe String
> partnersForename p = case partner p of
>   Just p  -> Just $ forename p
>   Nothing -> Nothing

Both functions share a common pattern: we compute the value of some expression
that can be either 'Nothing' or 'Just x'. In the former case, we want to return
'Nothing', while in the latter case we want to apply some function on 'x'. This
other function again returns a 'Maybe' type: a 'Nothing' or some value wrapped
up in 'Just'. The problem here is that we keep wrapping and unwrapping values
from of 'Maybe' types. This results in spaghetti code.

In these examples we deal with a CHAIN of computations, each of which can FAIL.
If the computation fails, it returns 'Nothing', otherwise it returns 'Just x'.
Then follows another computation, that again can fail or succeed, etc. 

  COMPUTATION1(x) --Just y--> COMPUTATION2(y) --Just z--> COMPUTATION3(z)
       |                           |                           |
     fails                       fails                       fails
       |                           |                           |
       v                           v                           v
    Nothing                     Nothing                     Nothing

We would like to have some abstraction machinery to make this chaining easier!

We can start by introducing two functions:

> inject :: a -> Maybe a
> inject x = Just x    -- or: inject = Just

The 'inject' functions just wraps up a value into a 'Just' type. (Actually, it
is identical to 'Just' but we introduce it here for didactic purposes.)

The other function is:

> bind :: Maybe a -> (a -> Maybe b) -> Maybe b
> bind Nothing _  = Nothing
> bind (Just x) k = k x

So, 'bind' links a value wrapped up into a 'Maybe' type with a function that
takes the unwrapped value and returns a new value of the 'Maybe' type.  Now, if
the first computation fails and returns 'Nothing', this is where the story
ends. Otherwise, we unwrap the value and apply to it the function 'k'.

We can now streamline our code:

> grandmothersPartner2 :: Person -> Maybe Person
> grandmothersPartner2 p = (mother p `bind` mother) `bind` partner

> partnersForename2 :: Person -> Maybe String
> partnersForename2 p = partner p `bind` (\r -> inject (forename r))

or shorter:

> partnersForename2' :: Person -> Maybe String
> partnersForename2' p = partner p `bind` (inject . forename)

The operations that we've just defined actually define a MONAD.

A monad is a TYPE CLASS of all polymorphic types that provide the BIND and
INJECT operations. The bind operation is defined as the (>>=) operator, while
injection is defined as the 'return' function.

The definition of the 'Monad' type class is as follows:

  class Monad m where
      (>>=)       :: m a -> (a -> m b) -> m b
      return      :: a -> m a

Note that the type constructor 'm' is of kind '* -> *'. This, for instance,
could be a 'Maybe' type constructor, or '[]', 'IO', or any user-defined type
constructor.

'Data.Maybe' module defines the instance of the 'Monad' type class for the
'Maybe' type. It is defined exactly as we defined it above:

  instance Monad Maybe where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing
    
    return              = Just

This means that we can define our functions as follows:

> grandmothersPartner3 :: Person -> Maybe Person
> grandmothersPartner3 p = (mother p >>= mother) >>= partner

> partnersForename3 :: Person -> Maybe String
> partnersForename3 p = partner p >>= return . forename

The (>>=) operator is defined with the following fixity and binding 
precedence:

  infixl 1  >>=

So it's left associative and binds very loosely. This means that we can write:

> grandmothersPartner4 :: Person -> Maybe Person
> grandmothersPartner4 p = mother p >>= mother >>= partner

Truth be told, the 'Monad' type class has two additional functions:

  class Monad m where
      (>>=)       :: m a -> (a -> m b) -> m b
      (>>)        :: m a -> m b -> m b
      return      :: a -> m a
      fail        :: String -> m a

However, (>>=) and 'return' are the minimal complete definition, thus you are
not required to define the two additional functions. Their definition defaults
to:

  m >> k = m >>= \_ -> k
  fail = error

The (>>) operator is similar to binding operator (>>=). The difference is that
its right argument is not a function of 'a -> m b' type, but a value of 'm b'
type. More informally, the (>>) operator does not forward the result of the
first computation to the second computation, but instead simply discards the
result of the first computation. We call (>>) the SEQUENCING operator.

For example,

> foo :: Person -> Maybe Person
> foo p = partner p >> mother p 

The (>>) operator will be useful for IO operations of type 'IO ()', which we
use solely because of their side effects and which have nothing to be passed
on. For 'Maybe' monad, this operator is not really useful.

(On a side note: the 'fail' operator is for errors that arise during pattern
matching. More on this later.)

To sum up:

A 'Maybe' monad is an abstraction for a chain of computations, each of which
can fail. If one of the computations fails, it returns a 'Nothing', and the
whole computation evaluates to 'Nothing'. Otherwise, if ALL computations
succeed, we get the result wrapped into a 'Just'.

Got it? Let's see. What is the result of the following computations?

> v1 = Just 5 >>= return . (+5)
> v2 = Just 5 >> return 6
> v3 = Just 5 >> Just 6 >> return 7 >>= return . (+1)
> v4 = return 0 >>= Just . (+1) >> return 2
> v5 = find isUpper "Haskell" >>= return . toLower
> v6 = Just 5 >>= return
> v7 = Just 5 >>= return >> return 6
> v8 = Just (2,3) >>= return . fst >>= return . (+1)
> v9 = Nothing >>= return . fst >>= return . (+1)
> v10 = Just 5 >>= const Nothing >>= return . (+1)
> v11 = Just 5 >> Nothing >>= return . (+1)
> v12 = Just 5 >> (Just 6 >> Just 7)
> v13 = (Just 5 >> Just 6) >> Just 7
> v14 = Just 3 >>= \x -> Just $ show x
> v15 = Just 3 >>= (\x -> Just (show x)) >>= return . (++"!")

== MONADIC LAWS ==============================================================

Each instance of a 'Monad' should abide by the three MONADIC LAWS. This laws
are not checked by the compiler (this cannot be done with type checking), so
you have to ensure that the laws hold. Otherwise, the instance will not be a
true monad and will not behave as expected. The three laws are:

  (1) return a >>= k           ==  k a
  (2) m >>= return             ==  m
  (3) m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h

The first law tells us that 'return' simply passes on its value to the next
computation. The second law tells us that, if we immediately return a value of
a computation 'm', this is the same as letting the computation 'm' return its
value on its own. In other words, it makes no sense to take the result of a
computation only to immediately return it. The third law is the associativity
of the binding operator (>>=). It means that the order in which we group the
computation is irrelevant.

As a special case of the third law, we have:

  m1 >> (m2 >> m3) = (m1 >> m2) >> m3

This then means that we can drop the parentheses altogether when sequencing
or binding operations, and simply write:

  m1 >> m2 >> m3

The third law is perhaps a bit confusing because it seems to suggest that the
ORDER OF COMPUTATION is irrelevant. But this is not the case. In a monad, the
order of computation is important! But what is not important is the GROUPING OF
COMPUTATIONS. As long as the order is the same, the grouping is irrelevant. 
Keep in mind: associativity does not change the order of computation (but
commutativity does). More on this here:
http://lambda-the-ultimate.org/node/2448

Now, why is this not working?

 v16 = return 0 >>= return . (+1)

Because 'return' and (>>=) are polymorphic. The compiler does not know what
type we want to have here, it only knows that it has to be an instance of the
'Monad' type class. We have to be more specific regarding the type, e.g.:

> v17 = return 0 >>= return . (+1) :: Maybe Int
> v18 = return 0 >>= return . (+1) :: IO Int

== EXERCISE 1 ================================================================

Define the following functions within a 'Maybe' monad.

1.1
- Define a function
  grandfathersPartnerForename :: Person -> Maybe String

1.2
- Using 'Data.List.stripPrefix', define a function
  stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
  stripSuffix "ar" "bumbar" => Just "bumb"
  stripSuffix "ak" "bumbar" => Nothing
- Define a function
  removeAffixes :: String -> String -> String -> Maybe String
  that removes the prefix and suffix from a given string, if possible.
  removeAffixes :: "bu" "ar" "bumbar" => Just "mb"

== IO MONAD ==================================================================

Let's now look at the most famous monad of all time: the IO monad.

The type constructor 'IO' is also an instance of the Monad type class.

Recall the functions:

  putStrLn :: String -> IO ()
  getLine :: IO String

We can write:

> hello1 :: IO ()
> hello1 = putStrLn "Hello" >>= \_ -> putStrLn "Hello!"

or, simpler:

> hello2 :: IO ()
> hello2 = putStrLn "Hello" >> putStrLn "Hello!"

Instead of writing it like this, we used a 'do' block. But here's a revelation:
the 'do' block is nothing more than a syntactic sugar. We could have done
without it, using (>>=) and 'return'.

For example, instead of:

> main1 :: IO ()
> main1 = do
>   putStrLn "Introduce tu número de la suerte"
>   number <- getLine
>   putStrLn $ "Lo creas o no, tu número de la suerte es " ++ number

we could have written:

> main2 :: IO ()
> main2 = 
>   putStrLn "Introduce tu número de la suerte" >>
>   getLine >>= 
>   (\number -> putStrLn $ "Lo creas o no, tu número de la suerte es " ++ number)

Similarly, instead of:

> askName1 :: IO String
> askName1 = do
>   putStrLn "Inserisci il tuo nome"
>   s1 <- getLine
>   putStrLn "Inserisci il tuo cognome"
>   s2 <- getLine
>   return $ s1 ++ " " ++ s2

we could have written:

> askName2 :: IO String
> askName2 = 
>   putStrLn "Inserisci il tuo nome" >>
>   getLine >>= (\s1 -> 
>     putStrLn "Inserisci il tuo cognome" >>
>     getLine >>= (\s2 -> 
>       return $ s1 ++ " " ++ s2))

This gets a bit more complicated in the presence of pattern matching that can
fail (refutable pattern matching), e.g., for patterns that consists of more
than one variable. For example:

> main3 = do
>   (x:_) <- getLine
>   putStrLn $ "Der erste Buchstabe ist " ++ [x]

This is equivalent to:

> main4 =
>   let ok (x:_) = putStrLn $ "Der erste Buchstabe ist " ++ [x]
>       ok _     = fail "pattern match failure"
>   in getLine >>= ok

In sum, the general "desugarization rules" for 'do' notations are:

  do e                        =>   e
  do e1; e2; ...; en          =>   e1 >> do e2; ...; en
  do x <- e1; e2; ...; en     =>   e1 >>= \x -> do e2; ...; en
  do pat <- e1; e2; ...; en   =>   let ok pat = do e2; ...; en
                                       ok _   = fail "..."
                                   in e1 >>= ok

The absolutely awesome thing is that the 'do' notation is not only applicable
to the IO monad. We can use it for any monad. For example, for 'Maybe':

Instead of:

  grandmothersPartner4 :: Person -> Maybe Person
  grandmothersPartner4 p = mother p >>= mother >>= partner

we can add some sugar:

> grandmothersPartner5 :: Person -> Maybe Person
> grandmothersPartner5 p = do
>   m <- mother p
>   g <- mother m
>   partner g

Let us now rephrase the three monadic laws in 'do' notation:
  
  (1) return a >>= k      ==  k a

      do y <- return x    ==  k x
         k y

  (2) m >>= return        ==  m

      do x <- m           ==  m
         return x
  
  (3) m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h

      do x <- m                ==  do y <- do x <- m
         y <- k x                             k x
         h y                          h y

== EXERCISE 2 ================================================================

2.1.
- Define the function 'grandfathersPartnerForename' from Problem 1.1 using 'do'
  notation.

2.2. 
- Desugarize this function:
  main5 :: IO ()
  main5 = do
    xs <- getArgs
    h <- case xs of
      (f:_) -> do e <- doesFileExist f
                  if e then openFile f ReadMode else return stdin
      []    -> return stdin
    s <- hGetContents h
    putStr . unlines . sort $ lines s

== NEXT ======================================================================

Next week we'll look into some more monads: the state monad and the list monad.
We'll also look into generic functions for working with monads.

