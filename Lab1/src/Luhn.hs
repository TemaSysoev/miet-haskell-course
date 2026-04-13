module Luhn where

-- Проверка корректности номера банковской карты алгоритмом Луна https://ru.wikipedia.org/wiki/Алгоритм_Луна.
-- Алгоритм:
-- 1. Все цифры, стоящие на чётных местах (считая с конца), удваиваются. Если при этом получается число, большее 9, то из него вычитается 9. Цифры, стояшие на нечётных местах, не изменяются.
-- То есть: последняя цифра не меняется; предпоследнее удваивается; 3-е с конца (предпредпоследнее) не меняется; 4-е с конца удваивается и т.д.
-- 2. Все полученные числа складываются.
-- 3. Если полученная сумма кратна 10, то исходный список корректен.

-- Не пытайтесь собрать всё в одну функцию, используйте вспомогательные.
-- Например: разбить число на цифры (возможно, сразу в обратном порядке).
-- Не забудьте добавить тесты, в том числе для вспомогательных функций!

isLuhnValid :: Int -> Bool

digits :: Int -> [Int]
digits n
    | n < 0 = digits (abs n)
    | n < 10 = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]

processDigits :: [Int] -> [Int]
processDigits = reverse . map processDigit . zip [0 :: Int ..] . reverse where
    processDigit (i, d)
        | odd i = doubleDigit d
        | otherwise = d

doubleDigit :: Int -> Int
doubleDigit d = let doubled = 2 * d in if doubled > 9 then doubled - 9 else doubled

isLuhnValid n = sum (processDigits (digits n)) `mod` 10 == 0
