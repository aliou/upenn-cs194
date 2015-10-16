{-
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW01 where         -- We'll learn more about this later

{--
01.
We first need to be able to break up a number into its last
digit and the rest of the number. Write these functions:
 lastDigit     :: Integer -> Integer
 dropLastDigit :: Integer -> Integer
If you’re stumped, look through some of the arithmetic operators mentioned
in the lecture.
Example: lastDigit 123 == 3
Example: lastDigit 0 == 0
Example: dropLastDigit 123 == 12
Example: dropLastDigit 5 == 0
--}

lastDigit :: Integer -> Integer
lastDigit n =
  n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n =
  (n - lastDigit n) `div` 10

{--
02.
Now, we can break apart a number into its digits. Define the function
toDigits should convert positive Integers to a list of digits.
  (For 0 or negative inputs, toDigits should return the empty list.)
Example: toDigits 1234 == [1,2,3,4]
Example: toDigits 0 == []
Example: toDigits (-17) == []
--}
--
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = (toDigits (dropLastDigit n)) ++ (lastDigit n) : []
