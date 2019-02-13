-- 1. В Haskell есть две функции для целочисленного деления: quot и div,
-- а также две функции для взятия остатка: rem и mod. Как отличается
-- их поведение, когда один из аргументов является отрицательным числом?

-- 2. Напишите функцию типа Float -> Float -> Float -> Float, которая
-- вычисляет меньший из корней уравнения |x - a| + |x - b| = c.

-- 3. Напишите функцию Integer -> Integer вычисляющую произведение
-- последних трех десятичных цифр числа.

-- 4. Напишите функцию Integer -> Char, которая переводит цифру в символ.
-- Функция должна вызывать error, если аргумент не из отрезка [0..9].

-- ---------------------------------------------------------------------

-- The quot, rem, div, and mod class methods satisfy these laws if y is non-zero:

-- (x `quot` y)*y + (x `rem` y) == x  
-- (x `div`  y)*y + (x `mod` y) == x
-- quot is integer division truncated toward zero, 
-- while the result of div is truncated toward negative infinity.

-- The div function is often the more natural one to use, 
-- whereas the quot function corresponds to the machine instruction 
-- on modern machines, so it's somewhat more efficient.

task1 :: Float -> Float -> Float -> Float 
task1 a b c = min (0.5 * (a + b + c)) (0.5 * (a + b - c))

task2 :: Integer -> Integer
task2 x = let a = div x 10
              b = div a 10
          in product (map (`mod` 10) [a, b, x])

chars = "0123456789"
-- task3 :: Integer -> Char
task3 x
    | 0 <= x && x <= 9 = (chars!!x)
    | otherwise = error "Out of digit range!"
