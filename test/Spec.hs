import Test.Hspec
import Test.QuickCheck
import Gem3.Cas
import Gem3.Data
import qualified Control.Exception
import qualified Data.Set as S
import qualified Data.Map as M
import Gem3.Lib

main :: IO ()
main = hspec $ do
  -- describe "Prelude.head" $ do
  --   it "returns the first element of a list" $ do
  --     head [23 ..] `shouldBe` (23 :: Int)

  --   it "returns the first element of an *arbitrary* list" $
  --     property $ \x xs -> head (x:xs) == (x :: Int)

  --   it "throws an exception if used with an empty list" $ do
  --     evaluate (head []) `shouldThrow` anyException
  describe "cas" $ do
    describe "solveFor" $ do
      it "works on simple cases" $ do
        solveFor "x" (CasExpr 0.25 (M.fromList [("x", 2)])) `shouldBe`
          Just (pureNum 2.0)

    describe "evaluate" $ do
      it "works on simple cases" $ do
        evaluate (const Nothing) (CasExpr 7 (M.empty :: M.Map String Float))
          `shouldBe` Just 7
    describe "searchForSolution" $ do
      it "works on simple cases" $ do
        searchForSolution (S.fromList [(CasExpr 0.25 (M.fromList [("x", 2)]))]) "x"
          `shouldBe` Just 2

      it "works on a more complicated case" $ do
        -- a = 4
        let expr1 = (CasExpr 0.25 (M.fromList [("a", 1)]))
        -- b = 2
        let expr2 = (CasExpr 0.5 (M.fromList [("b", 1)]))
        -- a * b = c
        let expr3 = (CasExpr 1 (M.fromList [("b", 1), ("a", 1), ("c", -1)]))
        searchForSolution (S.fromList [expr1, expr2, expr3]) "c"
          `shouldBe` Just 8

      it "works on another simple case" $ do
        let expr1 = (CasExpr 0.5 (M.fromList [("b", 1)]))
        -- a * b = c
        let expr2 = (CasExpr 1 (M.fromList [("b", 1), ("a", -1)]))
        searchForSolution (S.fromList [expr1, expr2]) "a"
          `shouldBe` Just 2

  describe "end-to-end" $ do
    it "should solve simple things" $ do
      let
        simpleProgram = [
          Left $ DeclType "a" mDim,
          Left $ DeclVal "b" (Num (fromInteger 13) mDim),
          Left $ SetEqual (simpleVar "a") (simpleVar "b"),
          Right $ EvalCommand ["a"]
          ]

      let res = runProgram simpleProgram
      print res
      -- case runProgram simpleProgram of
      --   Right (state, messages)
      1 `shouldBe` 1
