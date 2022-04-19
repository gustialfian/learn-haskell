doubleMe x = x + x

doubleUs x y = x * 2 + y * 2

doubleSmallNum x = if x > 100 then x else x * 2

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

triangles =
  [ (a, b, c)
    | c <- [1 .. 10],
      b <- [1 .. 10],
      a <- [1 .. 10],
      a ^ 2 + b ^ 2 == c ^ 2,
      a + b + c == 24
  ]

-- types-and-typeclasses
{-
  Everything before the => symbol is called a class constraint
  http://learnyouahaskell.com/types-and-typeclasses
-}

-- Syntax in Functions
{-
  http://learnyouahaskell.com/syntax-in-functions
-}

-- pattern matching
lucky 7 = "LCUKY NUMBER SEVEN!"
lucky x = "Sorry u out of luck, pal."

factorial 0 = 1
factorial n = n + factorial (n - 1)

addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first (x, _, _) = x

second (_, _, x) = x

third (_, _, x) = x

head' [] = error "Can't call head on an empty list, dummy"
head' (x : _) = x

tell [] = "the list is empty"
tell [x] = "the list has one element: " ++ show x
tell [x, y] = "the list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) = "this list is long. the first towl element are: " ++ show x ++ " and " ++ show y

length'' [] = 0
length'' (_ : xs) = 1 + length'' xs

capital "" = "empty string, ops"
capital all@(x : xs) = "the first letter of " ++ all ++ " is " ++ [x]

-- guards
bmiTell w h
  | w / h ^ 2 <= 18.5 = "you're underweight"
  | w / h ^ 2 <= 25.5 = "you're normal"
  | w / h ^ 2 <= 30.5 = "you're fat"
  | otherwise = "you're whale"

max' a b
  | a > b = a
  | otherwise = b

a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

-- where
bmiTell' w h
  | bmi <= skinny = "you're underweight"
  | bmi <= normal = "you're normal"
  | bmi <= fat = "you're fat"
  | otherwise = "you're whale"
  where
    bmi = w / h ^ 2
    skinny = 18.5
    normal = 25.5
    fat = 30.5

-- let
cylinder r h =
  let side = 2 * pi * r * h
      top = pi * r ^ 2
   in side + 2 * top


-- recursive
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)