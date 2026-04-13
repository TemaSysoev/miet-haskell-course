{-# LANGUAGE ScopedTypeVariables #-}

import FunctorsMonads
import Streams hiding (main)
import Test.Hspec
-- Раскомментируйте QuickCheck или Hegdehog, в зависимости от того, что будете использовать
-- Документация https://hspec.github.io/quickcheck.html
import Test.Hspec.QuickCheck
-- Документация в https://github.com/parsonsmatt/hspec-hedgehog#readme
-- import Test.Hspec.Hedgehog
import Test.QuickCheck

-- Добавьте минимум 5 тестов свойств для функций из первых 2 лабораторных (скопируйте определения тестируемых функций сюда).

genStream :: Arbitrary a => Gen (Stream a)
genStream = do
  x <- arbitrary
  xs <- genStream
  return (x :> xs)

instance Arbitrary a => Arbitrary (Stream a) where
  arbitrary = genStream

main :: IO ()
main = hspec $ do
  describe "Functor laws for Stream" $ do
    it "fmap id = id" $
      property $ \(xs :: Stream Int) ->
        sTake 100 (fmap id xs) == sTake 100 xs

    it "fmap (f . g) = fmap f . fmap g" $
      property $ \(xs :: Stream Int) ->
        let f = (+1)
            g = (*2)
        in sTake 100 (fmap (f . g) xs) == sTake 100 ((fmap f . fmap g) xs)

  describe "Applicative laws for Stream" $ do
    it "pure id <*> v = v" $
      property $ \(xs :: Stream Int) ->
        sTake 100 (pure id <*> xs) == sTake 100 xs

    it "pure f <*> pure x = pure (f x)" $
      property $ \(x :: Int) ->
        sTake 10 (pure (+1) <*> pure x) == sTake 10 (pure ((+1) x) :: Stream Int)

    it "u <*> pure y = pure ($ y) <*> u" $
      property $ \(y :: Int) ->
        let u = sCycle [(+1), (*2), subtract 3]
        in
        sTake 10 (u <*> pure y) == sTake 10 (pure ($ y) <*> u)

  describe "Monad laws for Stream" $ do
    it "return a >>= k = k a" $
      property $ \(a :: Int) (Fun _ k :: Fun Int (Stream Int)) ->
        sTake 50 (return a >>= k) == sTake 50 (k a)

    it "m >>= return = m" $
      property $ \(m :: Stream Int) ->
        sTake 50 (m >>= return) == sTake 50 m

    it "m >>= (\\x -> k x >>= h) = (m >>= k) >>= h" $
      property $ \(m :: Stream Int)
                   (Fun _ k :: Fun Int (Stream Int))
                   (Fun _ h :: Fun Int (Stream Int)) ->
        sTake 50 (m >>= (\x -> k x >>= h))
          == sTake 50 ((m >>= k) >>= h)

  describe "Stream functions" $ do
    it "sTake returns correct number of elements" $
      property $ \(n :: Positive Int) (x :: Int) ->
        length (sTake (getPositive n) (sRepeat x)) == getPositive n

    it "sInterleave alternates elements" $
      sTake 8 (sInterleave (sRepeat (1 :: Int)) (sRepeat 2)) `shouldBe` [1, 2, 1, 2, 1, 2, 1, 2]
