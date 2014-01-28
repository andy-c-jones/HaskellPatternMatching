module Main where

import Test.Hspec

data Tree = Empty
          | Leaf String
          | Branch Tree Tree
          deriving (Eq, Show)

kxy = Branch(Branch (Leaf "K") (Leaf "x")) (Leaf "y")

sxyz = Branch(Branch (Branch (Leaf "S") (Leaf "x")) (Leaf "y")) (Leaf "z")

execute :: Tree -> Tree
execute (Branch (Branch (Branch (Leaf "S") x) y) z) = (Branch (Branch (x) z) (Branch y z))
execute (Branch (Branch (Leaf "K") x) y) = x
execute (Branch x y) = Branch (execute x) (execute y)
execute l = l

main :: IO ()
main = hspec $ do
  describe "Prelude" $ do

    describe "(Kx)y" $ do
      it "returns x" $ do
        execute kxy `shouldBe` (Leaf "x")

    describe "((Sx)y)z" $ do
      it "returns (xz)(xy)" $ do
        execute sxyz `shouldBe` Branch (Branch (Leaf "x") (Leaf "z")) (Branch (Leaf "y") (Leaf "z"))

    describe "x" $ do
      it "returns x" $ do
        execute (Leaf "x") `shouldBe` (Leaf "x")

    describe "((Kx)y)((Kx)y)" $ do
      it "returns (xx)" $ do
        execute (Branch kxy kxy) `shouldBe` Branch (Leaf "x") (Leaf "x")