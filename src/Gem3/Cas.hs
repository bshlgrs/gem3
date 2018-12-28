module Gem3.Cas where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intercalate)
import Data.Maybe (catMaybes)
-- time to make yet another shitty computer algebra system

data CasExprOver numType x = CasExpr {
    constant :: numType,
    factors :: Map x numType
  }
  deriving (Eq, Ord)

pureNum :: numType -> CasExprOver numType a
pureNum n = CasExpr n M.empty

pureVar :: (Num a) => x -> CasExprOver a x
pureVar x = CasExpr (fromInteger 1) (M.singleton x (fromInteger 1))

instance (Eq n, Show a, Show n, Ord a, Num n) => Num (CasExprOver n a) where
  x + y = error $ "todo: addition. " <> show x <> " " <> show y
  (CasExpr k1 map1) * (CasExpr k2 map2)
    = CasExpr (k1 * k2) (M.unionWith (+) map1 map2)
  abs = undefined
  signum = undefined
  fromInteger n = CasExpr (fromInteger n) M.empty
  x - y = error $ "todo: subtraction. \n\n" <> show x <> "\n\n" <> show y

instance (Eq n, Show a, Show n, Ord a, Fractional n) => Fractional (CasExprOver n a) where
  fromRational = pureNum . fromRational
  recip (CasExpr k1 factors) = CasExpr (1/k1) (fmap negate factors)

getAllVars :: CasExprOver a b -> S.Set b
getAllVars (CasExpr _ f) = M.keysSet f

instance (Eq numType, Num numType, Show numType, Show x) => Show (CasExprOver numType x) where
  show (CasExpr k factors)
    | M.null factors  = show k
    | otherwise = show k <> " * " <>
                    intercalate " * "
                        (map
                          (\(k, p) ->
                             if p == fromInteger 1 then show k else show k <> "**" <> show p)
                          $ M.toList factors)

solveFor :: (Ord a, Floating num) => a -> CasExprOver num a -> Maybe (CasExprOver num a)
solveFor name (CasExpr k factors) = case M.lookup name factors of
  Nothing -> Nothing
  Just power -> let
    factorsWithNameRemoved = M.delete name factors
    inversePower = 1 / power
    in Just $ CasExpr (k ** (-inversePower)) (fmap (** inversePower) factorsWithNameRemoved)

evaluate :: (Ord a, Floating num) => (a -> Maybe num) -> CasExprOver num a -> Maybe num
evaluate f (CasExpr k factors) = do
  nums <- traverse (\(k, v) -> (**v) <$> f k) (M.toList factors)
  pure $ k * (foldr (*) 1 nums)

searchForSolution :: (Floating n, Ord n, Ord v) => S.Set (CasExprOver n v) -> v
                      -> Maybe n
searchForSolution equations var = let
  solutions = S.map (evalVarWithEquation var equations) equations
    in case catMaybes $ S.toList solutions of
    -- if there are multiple, check equalitys
    [x] -> Just x
    _ -> Nothing


evalVarWithEquation :: (Floating n, Ord n, Ord a) => a -> S.Set (CasExprOver n a)
  -> CasExprOver n a -> Maybe n
evalVarWithEquation var equations equation = do
    expression <- solveFor var equation
    evaluate (\name -> searchForSolution (S.delete equation equations) name) expression
