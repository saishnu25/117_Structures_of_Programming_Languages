-- CSci 117, Lab 1:  Introduction to Haskell

---------------- Part 1 ----------------

-- WORK through Chapters 1 - 3 of LYaH. Type in the examples and make
-- sure you understand the results.  Ask questions about anything you
-- don't understand! This is your chance to get off to a good start
-- understanding Haskell.

---- Part 1 Examples ----

{- 

1) -> a = 5 + 4
  -> a
  9
  
2) -> 2 * 8
  16

3) -> 5 / 2
  2.5

4) ->not True
  False

5) ->True && True
  True

6) ->"Test" == "test"
  False

7) -> 7 - 2.2
  4.8

8) -> min 11 (-1)
  -1

9) ->max 14 101
  101

10) ->pred 10 + min 1 2 - 2
  8

11) ->20 `div` 3
  6

12) C:\Users\Saishnu\Desktop\CSCI 117 - Lab 1>ghci demo.hs
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( demo.hs, interpreted )
Ok, one module loaded. 
*Main> doubleMe 3
6

13) C:\Users\Saishnu\Desktop\CSCI 117 - Lab 1>ghci demo.hs
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( demo.hs, interpreted )
Ok, one module loaded.
*Main> doubleUs 2 3
10

14) C:\Users\Saishnu\Desktop\CSCI 117 - Lab 1>ghci demo.hs
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( demo.hs, interpreted )
Ok, one module loaded.
*Main> doubleSmallNumber 20
40

15) Prelude> let lostNumbers = [1,2,3,4,5,6]
Prelude> lostNumbers
[1,2,3,4,5,6]

16) Prelude> [2,3,4,5] ++ [1,2,3,4]
[2,3,4,5,1,2,3,4]

17) Prelude> 'H': "ello"
"Hello"

18) Prelude> "Morning!" !! 3
'n'

19) Prelude> [5, 10, 15] > [1, 2, 3]
True

20) Prelude> [4, 6, 8] == [15, 5, 9]
False

-}

---- End of Part 1 Examples ----

---------------- Part 2 ----------------

-- The Haskell Prelude has a lot of useful built-in functions related
-- to numbers and lists.  In Part 2 of this lab, you will catalog many
-- of these functions.

-- Below is the definition of a new Color type (also called an
-- "enumeration type").  You will be using this, when you can, in
-- experimenting with the functions and operators below.
data Color = Red | Orange | Yellow | Green | Blue | Violet
     deriving (Show, Eq, Ord, Enum)

-- For each of the Prelude functions listed below, give its type,
-- describe briefly in your own words what the function does, answer
-- any questions specified, and give several examples of its use,
-- including examples involving the Color type, if appropriate (note
-- that Color, by the deriving clause, is an Eq, Ord, and Enum type).
-- Include as many examples as necessary to illustration all of the
-- features of the function.  Put your answers inside {- -} comments.
-- I've done the first one for you (note that "λ: " is my ghci prompt).


-- succ, pred ----------------------------------------------------------------

{- 
succ :: Enum a => a -> a
pred :: Enum a => a -> a

For any Enum type, succ gives the next element of the type after the
given one, and pred gives the previous. Asking for the succ of the
last element of the type, or the pred of the first element of the type
results in an error.

λ: succ 5
6
λ: succ 'd'
'e'
λ: succ False
True
λ: succ True
*** Exception: Prelude.Enum.Bool.succ: bad argument
λ: succ Orange
Yellow
λ: succ Violet
*** Exception: succ{Color}: tried to take `succ' of last tag in enumeration
CallStack (from HasCallStack):
  error, called at lab1.hs:18:31 in main:Main
λ: pred 6
5
λ: pred 'e'
'd'
λ: pred True
False
λ: pred False
*** Exception: Prelude.Enum.Bool.pred: bad argument
λ: pred Orange
Red
λ: pred Red
*** Exception: pred{Color}: tried to take `pred' of first tag in enumeration
CallStack (from HasCallStack):
  error, called at lab1.hs:18:31 in main:Main
-}


-- toEnum, fromEnum, enumFrom, enumFromThen, enumFromTo, enumFromThenTo -------
-- As one of your examples, try  (toEnum 3) :: Color --------------------------

{-

      -- toEnum --

Prelude> :type toEnum
toEnum :: Enum a => Int -> a

- Description:
toEnum would give the result that is set in the specific position that the result is in the list.
You would need to specify what result would you want to call from the list. If the number/input you
entered cannot be found in the list, it would output an error message. 

- Examples: 
1) *Main> toEnum 3 :: Color
Green

2) *Main> toEnum 4 :: Name
Gabe

3) *Main> toEnum 6 :: Color
*** Exception: toEnum{Color}: tag (6) is outside of enumeration's range (0,5)
CallStack (from HasCallStack):
  error, called at demo.hs:10:31 in main:Main


      -- fromEnum --

Prelude> :type fromEnum
fromEnum :: Enum a => a -> Int

- Description:
fromEnum looks like the opposite of toEnum where you would input a certain string/character
and it would output the position that the requested input is located within the list. If 
what you are looking for is not present in the list, it would output an error stating that
it is not within the scope. 

- Examples:
1) *Main> fromEnum Blue
4

2) *Main> fromEnum Adam
1

3) *Main> fromEnum Gold
<interactive>:9:10: error: Data constructor not in scope: Gold


      -- enumFrom --

Prelude> :type enumFrom
enumFrom :: Enum a => a -> [a]

- Description:
enumFrom would take the input of the user, in this case, a type of color from the list, and it would 
output the list starting from the inputted color position and show the succeeding list of colors on
the list. If the input can not be found, it would state that it is not within its scope. 

- Examples:
1) *Main> enumFrom Blue
[Blue,Violet]

2) *Main> enumFrom Henry
[Henry,Adam,Yelena,Gabe,Ian,Sam]

3)*Main> enumFrom Pink
<interactive>:3:10: error: Data constructor not in scope: Pink


      -- enumFromThen --

Prelude> :type enumFromThen
enumFromThen :: Enum a => a -> a -> [a]

- Description:
enumFromThen takes whatever the user has inputted from and to and if it is within the list, it would show the 
result of both inputs that can be found within the list.If either one or both of the input is not found in 
the list, it would cause an error.

- Examples:
1) *Main> enumFromThen Green Violet
[Green,Violet]

2) *Main> enumFromThen Sam Henry
[Sam,Henry]

3) *Main> enumFromThen Green Gold
<interactive>:9:20: error:
    Data constructor not in scope: Gold :: Color

4) *Main> enumFromThen Gold Brown
<interactive>:10:14: error: Data constructor not in scope: Gold
<interactive>:10:19: error: Data constructor not in scope: Brown


      -- enumFromTo --

Prelude> :type enumFromTo
enumFromTo :: Enum a => a -> a -> [a]

- Description:
enumFromTo woudl output a list of elements that are from one element to another element that
the user has requested. For example: A, B, C, D, the user requests only from B to D therefore
it would output B, C, D only. It would have to be in order they were listed, it can not be done
the other way around where if it's from D to B, it causes an error. Also, if either one name is 
not on the list, it would result in an error. 

- Examples:
1) *Main> enumFromTo Green Violet
[Green,Blue,Violet]

2) *Main> enumFromTo Yelena Sam
[Yelena,Gabe,Ian,Sam]

3) *Main> enumFrom Ian Sam
<interactive>:8:1: error:
    * Couldn't match expected type `Name -> t'
                  with actual type `[Name]'
    * The function `enumFrom' is applied to two arguments,
      but its type `Name -> [Name]' has only one
      In the expression: enumFrom Ian Sam
      In an equation for `it': it = enumFrom Ian Sam
    * Relevant bindings include it :: t (bound at <interactive>:8:1)

4) *Main> enumFromTo Yelena Dan
<interactive>:11:19: error:
    * Data constructor not in scope: Dan :: Name
    * Perhaps you meant one of these:
        `Ian' (line 12), variable `tan' (imported from Prelude)


      -- enumFromThenTo --

Prelude> :type enumFromThenTo
enumFromThenTo :: Enum a => a -> a -> a -> [a]

- Description:
enumFromThenTo would take the first element then the second chosen element until the last element
that was inputted by the user. If a element is not found, it would be an error. 

- Examples:
1) *Main> enumFromThenTo Red Green Violet
[Red,Green]

2) *Main> enumFromThenTo Henry Adam Sam
[Henry,Adam,Yelena,Gabe,Ian,Sam]

3) *Main> enumFromThenTo Henry Ian Adam
[Henry]

4) *Main> enumFromThenTo Henry Ian Alex
<interactive>:19:26: error:
    * Data constructor not in scope: Alex :: Name
    * Perhaps you meant variable `lex' (imported from Prelude)

-}

-- ==, /= ---------------------------------------------------------------------

{-

Prelude> :type ==
<interactive>:1:1: error: parse error on input `=='

Prelude> :type /=
<interactive>:1:1: error: parse error on input `/='

- Description:
This operator symbol, ==,  stands for is equal to therefore if both variables on both sides
are the same, it would show if it's true or false. /= stands for not equal to, which is the
opposite to == therefore if either one variable on either side is not the same, it woudl also
result in a boolean output. 

- Examples:

1) Prelude> 1 == 1
True

2) Prelude> 2 /= 1
True

3) Prelude> True == False
False

4) Prelude> True /= False
True

-}

-- quot, div (Q: what is the difference? Hint: negative numbers) --------------

{-

Prelude> :type quot
quot :: Integral a => a -> a -> a

Prelude> :type div
div :: Integral a => a -> a -> a

- Description: 
Both functions do a similar thing whereby the both deal with division, that is to divide 
two variables together and return the value without the remainder. The quot would divide
and omit the remainder whereas div normally returns the number of times the value can be 
divided again. 

- Examples: 

1) Prelude> div 5 2
2

2) Prelude> quot 6 4
1

3) Prelude> div 2 5
0

4) Prelude> quot 2 5
0


-}


-- rem, mod  (Q: what is the difference? Hint: negative numbers) --------------

{-

Prelude> :type rem
rem :: Integral a => a -> a -> a

Prelude> :type mod
mod :: Integral a => a -> a -> a

- Description: 
Rem returns the remainder of the variables that have been inputted. Mod also does
a similar thing but it is a modulus. When mod is used with a negative number, as seen
below, the result is different as compared to when using rem. 

- Examples: 

1) Prelude> mod 10 (-3)
-2

2) Prelude> rem 10 (-3)
1

-}

-- quotRem, divMod ------------------------------------------------------------

{-
Prelude> :type quotRem
quotRem :: Integral a => a -> a -> (a, a)

Prelude> :type divMod
divMod :: Integral a => a -> a -> (a, a)

- Description:
quotRem returns the division result as well as the remainder of the values. divMod 
does a similar thing but instead of returning the remainder, it returns the modulus
of the values. 

- Examples: 
1) Prelude> divMod 5 2
(2,1)

2) Prelude> quotRem 5 2
(2,1)

3) Prelude> quotRem 10 3
(3,1)

4) Prelude> divMod 10 3
(3,1)

-}

-- &&, || ---------------------------------------------------------------------

{-
Prelude> :type &&
<interactive>:1:1: error: parse error on input `&&'

Prelude> :t and
and :: Foldable t => t Bool -> Bool

Prelude> :type ||
<interactive>:1:1: error: parse error on input `||'

Prelude> :t or
or :: Foldable t => t Bool -> Bool

- Description: 
These operators are the same as in high level languages, logical AND and OR. 

- Examples: 

1) Prelude> True && True
True

Prelude> False && True
False

Prelude> False && False
False

2) Prelude> True || True
True

Prelude> True || False
True

Prelude> False || False
False

-}

-- ++ -------------------------------------------------------------------------

{-

Prelude> :type ++
<interactive>:1:1: error: parse error on input `++'

- Description:
++ would combine two different lists together to make it into one single list. 

- Examples:
1) Prelude> ['a','b'] ++ ['c','d']
"abcd"

2) Prelude> [1,2,3,4] ++ [5,6,7,8]
[1,2,3,4,5,6,7,8]


-}

-- compare --------------------------------------------------------------------

{-

Prelude> :type compare
compare :: Ord a => a -> a -> Ordering

- Description:
It compares the values entered and see if it's Greater Than (GT), is Equal (EQ), or Less Than (LT).

- Examples:

1) Prelude> compare 5 3
GT

2) Prelude> compare 0 0
EQ

3) Prelude> compare 'a' 'b'
LT

-}

-- <, > -----------------------------------------------------------------------

{-

Prelude> :type <
<interactive>:1:1: error: parse error on input `<'

Prelude> :type >
<interactive>:1:1: error: parse error on input `>'

- Description:
< is less than, > is greater than. It is the same symbols that are used in high
level programming as well. It outputs a boolean result, it being True or False.

- Examples:

1) Prelude> 5 < 2
False

2) Prelude> 2 > 1
True

-}

-- max, min -------------------------------------------------------------------

{-

Prelude> :type max
max :: Ord a => a -> a -> a

Prelude> :type min
min :: Ord a => a -> a -> a

- Description:
max and in compares both values entered and results either value being the max
or the min of the second value in the function.

- Examples:

1) Prelude> min 4 0
0

2) Prelude> max 5 2
5

-}

-- ^ --------------------------------------------------------------------------

{-

Prelude> :type ^
<interactive>:1:1: error: parse error on input `^'

- Description:
^ means to the power of, like in math where you would take 3 ^ 2, which means
3 * 3. 

- Examples:

1) Prelude> 3 ^ 3
27

2)Prelude> 5^2
25

-}

-- concat ---------------------------------------------------------------------

{-

Prelude> :type concat
concat :: Foldable t => t [a] -> [a]

- Description:
This would basically concatenate two sub lists into one list.

- Examples:

1) Prelude> concat [['a','b'],['c','d']]
"abcd"

2) Prelude> concat [[1, 2, 3],[4]]
[1,2,3,4]

-}

-- const ----------------------------------------------------------------------

{-

Prelude> :type const
const :: a -> b -> a

- Description:
It looks like it sets a lower bound between two values. 

- Examples:

1) Prelude> const 3 6
3

2) Prelude> const (2/5) 3
0.4

-}

-- cycle ----------------------------------------------------------------------

{-

Prelude> :type cycle
cycle :: [a] -> [a]

- Description:
This is like a loop system where it repeats the list infinitly. 

- Examples: 

1) Prelude> cycle [1,3]
[1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3, ...] 

2) Prelude> cycle ['a','d','i']
"adiadiadiadiadiadiadiadiadiadiadiadiadiadiadiadiadiadiadi..."

-}

-- drop, take -----------------------------------------------------------------

{-
Prelude> :type drop
drop :: Int -> [a] -> [a]

Prelude> :type take
take :: Int -> [a] -> [a]

- Description:
Drop will drop a certain number of elements that are in the list depending on how many
the user entered. Take will take the first number of elements that the user has entered.

- Examples:

1) Prelude> drop 8 [1,2,3,4,5,6,7,8,9,10]
[9,10]

2) Prelude> take 2 ['h','e','l','l','o']
"he"

-}

-- elem -----------------------------------------------------------------------
{-
Prelude> :type elem
elem :: (Foldable t, Eq a) => a -> t a -> Bool

- Description:
It would locate the element that has been requested to be found within the list 
of the function. 

- Examples:

1) Prelude> elem 'a' ['b','a','d','e']
True

2) Prelude> elem 10 [13, 5, 3, 22, 15]
False

-}

-- even -----------------------------------------------------------------------
{-
Prelude> :type even
even :: Integral a => a -> Bool

- Description:
It would output True if the number is even and False if the number is odd. 

- Examples:
1) Prelude> even 25
False

2) Prelude> even 30
True


-}

-- fst ------------------------------------------------------------------------

{-
Prelude> :type fst
fst :: (a, b) -> a

- Description:
It returns the first element in a tuple. 

- Examples:

1) Prelude> fst (3, 4)
3

2) Prelude> fst ("hi", "hello")
"hi"

-}

-- gcd ------------------------------------------------------------------------

{-
Prelude> :type gcd
gcd :: Integral a => a -> a -> a

- Description:
It takes the biggest common divisor. 

- Examples:

1) Prelude> gcd 10 3
1

2) Prelude> gcd 5 5
5

-}

-- head -----------------------------------------------------------------------

{-
Prelude> :type head
head :: [a] -> a

- Description:
It takes the head/first element, of the list. 

- Examples:

1) Prelude> head ['b','g','h','i']
'b'

2) Prelude> head [44, 76, 1, 2, 15]
44

-}

-- id -------------------------------------------------------------------------

{-
Prelude> :type id
id :: a -> a

- Description:
It takes the identity of the element in the function. 

- Examples:

1) Prelude> id [15, 6]
[15,6]

2) Prelude> id "CSCI117"
"CSCI117"

-}

-- init -----------------------------------------------------------------------

{-
Prelude> :type init
init :: [a] -> [a]

- Description:
It takes the list and outputs the list without the last element that is in the
list. 

- Examples:

1) Prelude> init [3,4,5,6,7,8]
[3,4,5,6,7]

2) Prelude> init ["abc","dce","ghi","jkl"]
["abc","dce","ghi"]

-}

-- last -----------------------------------------------------------------------

{-
Prelude> :type last
last :: [a] -> a

- Description:
This returns the last item in the list in the function. 

- Examples:
1) Prelude> last [3,2,1,0]
0

2) Prelude> last ["hello", "there!"]
"there!"

-}

-- lcm ------------------------------------------------------------------------

{-
Prelude> :type lcm
lcm :: Integral a => a -> a -> a

- Description:
This would get the lowest common multiple between two elements. 

- Examples:

1) Prelude> lcm 14 7
14

2) Prelude> lcm 30 5
30 

-}

-- length ---------------------------------------------------------------------

{-
Prelude> :type length
length :: Foldable t => t a -> Int

- Description:
This returns the number of items in a list. 

- Examples:

1) Prelude> length [2,4,6,8,10]
5

2) Prelude> length ['d','e','h']
3

-}

-- null -----------------------------------------------------------------------

{-
Prelude> :type null
null :: Foldable t => t a -> Bool

- Description:
If a list is empty, it will be null therefore it would output in boolean, True.
When there are elements in the list, it would output False.

- Examples:

1) Prelude> null[]
True

2) Prelude> null[1,2,3,4,5]
False

-}

-- odd ------------------------------------------------------------------------

{-
Prelude> :type odd
odd :: Integral a => a -> Bool

- Description:
It will check if the number is odd or not. 

- Examples:

1) Prelude> odd 33
True

2) Prelude> odd 24
False

-}

-- repeat ---------------------------------------------------------------------

{-
Prelude> :type repeat
repeat :: a -> [a]

- Description:
This is also like a loop where it will repeat the item infinitely. 

- Examples:

1) Prelude> repeat 'D'
"DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD..."

2) Prelude> repeat 25
[25,25,25,25,25,25,25,25,25,25,25,25,25...]

-}

-- replicate ------------------------------------------------------------------

{-
Prelude> :type replicate
replicate :: Int -> a -> [a]

- Description:
It creates a list and replicated the second item as per the number of times 
requested by the first item. 

- Examples:

1) Prelude> replicate 3 "hello"
["hello","hello","hello"]

2) Prelude> replicate 10 2
[2,2,2,2,2,2,2,2,2,2]
-}

-- reverse --------------------------------------------------------------------

{-
Prelude> :type reverse
reverse :: [a] -> [a]

- Description:
It takes the current list and reverses the order in which it is shown.

- Examples:

1) Prelude> reverse [10..20]
[20,19,18,17,16,15,14,13,12,11,10]

2) Prelude> reverse ['a'..'d']
"dcba"

-}

-- snd ------------------------------------------------------------------------

{-
Prelude> :type snd
snd :: (a, b) -> b

- Description:
It returns the second item in the tuple. 

- Examples:

1) Prelude> snd (13, 4)
4

2) Prelude> snd ('a','g')
'g'

-}

-- splitAt --------------------------------------------------------------------

{-
Prelude> :type splitAt
splitAt :: Int -> [a] -> ([a], [a])

- Description:
This basically breaks a list apart from a specified item.

- Examples:

1) Prelude> splitAt 6 [1,2,3,4,5,6,7,8,9,10]
([1,2,3,4,5,6],[7,8,9,10])

2) Prelude> splitAt 6 [2,4,6,8,10,12]
([2,4,6,8,10,12],[])
-}

-- zip ------------------------------------------------------------------------

{-
Prelude> :type zip
zip :: [a] -> [b] -> [(a, b)]

- Description:
Creates a list of tuples from two different lists with placing each item in 
its respective position. 

- Examples:

1) Prelude> zip [2,4,6] [1,3,5]
[(2,1),(4,3),(6,5)]

2) Prelude> zip ['a','b','c'] ['d','e','f']
[('a','d'),('b','e'),('c','f')]
-}


-- The rest of these are higher-order, i.e., they take functions as
-- arguments. This means that you'll need to "construct" functions to
-- provide as arguments if you want to test them.

-- all, any -------------------------------------------------------------------

{-
Prelude> :type all
all :: Foldable t => (a -> Bool) -> t a -> Bool

Prelude> :type any
any :: Foldable t => (a -> Bool) -> t a -> Bool

- Description:
All will return a boolean depending on the condition set by the user and if the list
complies with the condition as a whole. Any will return a boolean if any one of the 
items can be found within the list. 

- Examples:

1) Prelude> all (>11) [12,13,14,15]
True

2 Prelude> any (>11) [10,9,12,1]
True

3) Prelude> all (==1) [1,13,14,15]
False

4) Prelude> any odd [2,4,6,8,10]
False

-}

-- break ----------------------------------------------------------------------
{-
Prelude> :type break
break :: (a -> Bool) -> [a] -> ([a], [a])

- Description:
It will break and create a tuple to seperate the list at a certain point. 

- Examples:

1) Prelude> break (0 ==) [1,2,3,4,5]
([1,2,3,4,5],[])

2) Prelude> break ('e'==) ['t','e','s','t']
("t","est")


-}

-- dropWhile, takeWhile -------------------------------------------------------
{-
Prelude> :type dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]

Prelude> :type takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]

- Description:
dropWhile will check the condition set by the first argument and accordingly
acts to meet those conditions. takeWhile does a similar thing but stops once 
the condition does not meet. 

- Examples:

1) Prelude> dropWhile odd [1,2,3,4,5]
[2,3,4,5]

2) Prelude> dropWhile (>5) [11,5,10,8,9]
[5,10,8,9]

3) Prelude> takeWhile even [2,4,6,8,1]
[2,4,6,8]

4) Prelude> takeWhile odd [1,3,5,7,2,3,4]
[1,3,5,7]

-}

-- filter ---------------------------------------------------------------------
{-
Prelude> :type filter
filter :: (a -> Bool) -> [a] -> [a]

- Description:
This returns a list by considering the condition that was set by the first 
argument in the function. 

- Examples:

1) Prelude> filter even [1,2,3,4,5,6,7,8,9,10]
[2,4,6,8,10]

2) Prelude> filter odd [11,12,13,14,151,6,17,18,19,20]
[11,13,151,17,19]

-}

-- iterate --------------------------------------------------------------------
{-
Prelude> :type iterate
iterate :: (a -> a) -> a -> [a]

- Description:
Iterate will infintely create a list where the first argument in the function
applies to the second argument by doing some form of calculation.

- Examples:

1) Prelude> iterate (3*) (1)
[1,3,9,27,81,243...]

2)Prelude> iterate (10+) 1
[1,11,21,31,41,51]

-}

-- map ------------------------------------------------------------------------
{-
Prelude> :type map
map :: (a -> b) -> [a] -> [b]

- Description:
This would return a list from applying the first argument in the function to items
located within the second argument. 

- Examples:

1) Prelude> map (2*) [1,2,3,4]
[2,4,6,8]

2) Prelude> map (+10) [10,11,12,13]
[20,21,22,23]


-}

-- span -----------------------------------------------------------------------
{-
Prelude> :type span
span :: (a -> Bool) -> [a] -> ([a], [a])

- Description:
Span takes the condition set by the first argument and returns a tuple where it 
splits the list of items as per the condition that was set. 

- Examples:
1) Prelude> span (<5) [3,4,5,6,7,8]
([3,4],[5,6,7,8])

2) Prelude> span even [2,3,4,5,6,7]
([2],[3,4,5,6,7])

-}