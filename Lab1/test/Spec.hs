import Control.Exception (evaluate)
import FirstSteps
import Lists
import Luhn
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "first steps" $ do
        -- Можно вложить глубже: describe "xor" do $ ... чтобы дать названия отдельным тестам
        it "xor" $ do
            xor True True `shouldBe` False
            xor True False `shouldBe` True
            xor False True `shouldBe` True
            xor False False `shouldBe` False

        it "max3" $ do
            max3 1 3 2 `shouldBe` 3
            max3 5 2 5 `shouldBe` 5

        it "min3" $ do
            min3 1 3 2 `shouldBe` 1
            min3 5 2 5 `shouldBe` 2

        it "median3" $ do
            median3 1 3 2 `shouldBe` 2
            median3 5 2 5 `shouldBe` 5
            median3 9 1 4 `shouldBe` 4

        it "rbgToCmyk" $ do
            rbgToCmyk (RGB 255 0 0) `shouldBe` CMYK 0 1 1 0
            rbgToCmyk (RGB 0 0 0) `shouldBe` CMYK 0 0 0 1

        it "geomProgression" $ do
            geomProgression 3.0 2.0 2 `shouldBe` 12.0
            geomProgression 3.0 2.0 0 `shouldBe` 3.0
            geomProgression 8.0 2.0 (-2) `shouldBe` 2.0

        it "coprime" $ do
            coprime 10 15 `shouldBe` False
            coprime 12 35 `shouldBe` True
            coprime 0 1 `shouldBe` True
            coprime (-12) 35 `shouldBe` True

    describe "lists" $ do
        it "distance" $ do
            distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) `shouldBeCloseTo` sqrt 2.0
            distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) `shouldBeCloseTo` 1.0

        it "distance throws on different dimensions" $
            evaluate (distance (Point [1.0]) (Point [1.0, 2.0])) `shouldThrow` anyErrorCall

        it "intersect" $ do
            intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] `shouldBe` [2, 4]
            intersect [1, 2, 4, 6] [3, 5, 7] `shouldBe` []
            intersect [1, 2, 2, 3] [2] `shouldBe` [2, 2]

        it "zipN" $ do
            zipN ([[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Int]]) `shouldBe` [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
            zipN ([[1, 2, 3], [4, 5], [6]] :: [[Int]]) `shouldBe` [[1, 4, 6], [2, 5], [3]]
            zipN ([] :: [[Int]]) `shouldBe` []

        it "find" $ do
            find (> (0 :: Int)) [-1, 2, -3, 4] `shouldBe` Just 2
            find (> (0 :: Int)) [-1, -2, -3] `shouldBe` Nothing

        it "findLast" $ do
            findLast (> (0 :: Int)) [-1, 2, -3, 4] `shouldBe` Just 4
            findLast (> (0 :: Int)) [-1, -2, -3] `shouldBe` Nothing

        it "mapFuncs" $
            mapFuncs [\x -> x * x, (1 +), \x -> if even x then 1 else 0] (3 :: Int) `shouldBe` [9, 4, 0]

        it "satisfiesAll" $ do
            satisfiesAll [even, \x -> x `rem` 5 == 0] (10 :: Int) `shouldBe` True
            satisfiesAll [even, \x -> x `rem` 5 == 0] (12 :: Int) `shouldBe` False
            satisfiesAll [] (4 :: Int) `shouldBe` True

        it "tailNel" $
            tailNel (NEL 1 [2, 3] :: NEL Int) `shouldBe` [2, 3]

        it "lastNel" $ do
            lastNel (NEL 1 [2, 3]) `shouldBe` (3 :: Int)
            lastNel (NEL 1 []) `shouldBe` (1 :: Int)

        it "zipNel" $
            zipNel (NEL 1 [2, 3] :: NEL Int) (NEL 'a' ['b']) `shouldBe` NEL (1, 'a') [(2, 'b')]

        it "listToNel" $ do
            listToNel ([] :: [Int]) `shouldBe` Nothing
            listToNel [1, 2, 3 :: Int] `shouldBe` Just (NEL 1 [2, 3])

        it "nelToList" $
            nelToList (NEL 1 [2, 3]) `shouldBe` ([1, 2, 3] :: [Int])

    describe "luhn" $ do
        it "digits" $ do
            digits 0 `shouldBe` [0]
            digits 12345 `shouldBe` [1, 2, 3, 4, 5]
            digits (-123) `shouldBe` [1, 2, 3]

        it "doubleDigit" $ do
            doubleDigit 3 `shouldBe` 6
            doubleDigit 7 `shouldBe` 5

        it "processDigits" $ do
            processDigits [7, 9, 9, 2, 7, 3, 9, 8, 7, 1, 3] `shouldBe` [7, 9, 9, 4, 7, 6, 9, 7, 7, 2, 3]
            processDigits [0] `shouldBe` [0]

        it "isLuhnValid" $ do
            isLuhnValid 79927398713 `shouldBe` True
            isLuhnValid 79927398714 `shouldBe` False
            isLuhnValid 0 `shouldBe` True

shouldBeCloseTo :: Double -> Double -> Expectation
shouldBeCloseTo actual expected =
    abs (actual - expected) `shouldSatisfy` (< 1e-9)
