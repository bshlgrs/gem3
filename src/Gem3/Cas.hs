module Gem3.Cas where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intercalate)
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
  show (CasExpr k factors) = show k <> " * " <>
                    intercalate " * "
                        (map
                          (\(k, p) ->
                             if p == fromInteger 1 then show k else show k <> "**" <> show p)
                          $ M.toList factors)
