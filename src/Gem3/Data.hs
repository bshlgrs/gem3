module Gem3.Data where
import Data.Map (Map, fromList)
import qualified Data.Map as M
import Data.Group
import Gem3.Cas
import Data.List (intercalate)

type Path = [String]

data VariableRef
  = PathRef [String]
  | RelationVarRef RelationId Path
  deriving (Eq, Ord)

instance Show VariableRef where
  show = \case
    PathRef strings -> intercalate "@" strings
    RelationVarRef id path -> "@Rel-" <> show id <> "@" <> intercalate "@" path

simpleName :: String -> VariableRef
simpleName x = PathRef [x]

data Unit = Meter | Kilogram | Second
  deriving (Show, Eq, Ord)

data Dimension = Dimension (Map Unit Number)
  deriving (Eq, Ord)

instance Show Dimension where
  show (Dimension m) = intercalate "*" (map
      (\(dim, power) -> if power == 1 then show dim else show dim <> "**" <> show power)
      (M.toList m))

data Stmt
  = DeclVal String Expr
  | DeclType String Dimension
  | SetEqual Expr Expr
  | ModuleStmt String Module
  deriving (Show, Eq, Ord)

data Expr
  = Num Number Dimension
  | PathRefExpr Path
  | FuncCall Path [Expr]
  | FieldExpr ModuleExpr String
  deriving (Show, Eq, Ord)

data ModuleExpr
  = ModuleNameExpr Path
  | ModuleCallExpr Path (Map Path Expr)
  deriving (Show, Eq, Ord)

type Value = CasExprOver Number VariableRef

type RelationId = Int

data Number = SimpleNumber Float
  deriving (Eq, Ord)

instance Show Number where
  show (SimpleNumber f) = show f

data Command = EvalCommand Path
             | PrintStrCommand String
             | PrintExprCommand Expr
  deriving (Show, Eq, Ord)

type TopLevelStmt = Either Stmt Command
type Program = [TopLevelStmt]

data Module = Module [Stmt]
  deriving (Show, Eq, Ord)

type InterpreterError = String

kgDim = Dimension $ fromList [(Kilogram, SimpleNumber 1.0)]
mDim = Dimension $ fromList [(Meter, SimpleNumber 1.0)]
sDim = Dimension $ fromList [(Second, SimpleNumber 1.0)]
joule = kgDim <> (pow mDim 2) <> (pow sDim (-2))

simpleVar x = PathRefExpr [x]

numPow :: (Fractional a) => a -> Int -> a
numPow num n
  | n == 0 = fromInteger 1
  | n > 0 = num * numPow num (n - 1)
  | n < 0 = numPow num (n + 1) / num


instance Num Expr where
  x + y = FuncCall ["+"] [x, y]
  x * y = FuncCall ["*"] [x, y]
  negate x = FuncCall ["*"] [fromInteger (-1), x]
  abs x = FuncCall ["abs"] [x]
  signum x = FuncCall ["signum"] [x]
  fromInteger x = Num (SimpleNumber $ fromInteger x) (Dimension M.empty)

instance Fractional Expr where
  fromRational x = Num (fromRational x) (Dimension M.empty)
  recip expr = FuncCall ["/"] [fromInteger 1, expr]

instance Num Number where
  (SimpleNumber x) + (SimpleNumber y) = SimpleNumber $ x + y
  (SimpleNumber x) * (SimpleNumber y) = SimpleNumber $ x * y
  negate (SimpleNumber x) = SimpleNumber $ - x
  abs (SimpleNumber x) = SimpleNumber $ abs x
  signum (SimpleNumber x) = SimpleNumber $ signum x
  fromInteger x = SimpleNumber $ fromInteger x

instance Fractional Number where
  fromRational = SimpleNumber . fromRational
  recip (SimpleNumber n) = SimpleNumber $ recip n

instance Floating Number where
  pi = SimpleNumber pi
  exp (SimpleNumber x) = SimpleNumber $ exp x
  log (SimpleNumber x) = SimpleNumber $ log x
  sin (SimpleNumber x) = SimpleNumber $ sin x
  cos (SimpleNumber x) = SimpleNumber $ cos x
  asin (SimpleNumber x) = SimpleNumber $ asin x
  acos (SimpleNumber x) = SimpleNumber $ acos x
  atan (SimpleNumber x) = SimpleNumber $ atan x
  sinh (SimpleNumber x) = SimpleNumber $ sinh x
  cosh (SimpleNumber x) = SimpleNumber $ cosh x
  asinh (SimpleNumber x) = SimpleNumber $ asinh x
  acosh (SimpleNumber x) = SimpleNumber $ acosh x
  atanh (SimpleNumber x) = SimpleNumber $ atanh x

instance Semigroup Dimension where
  (Dimension m) <> (Dimension m2) = Dimension $ M.unionWith (+) m m2
instance Monoid Dimension where
  mempty = Dimension (M.empty)
instance Group Dimension where
  invert (Dimension m) = Dimension $ fmap negate m
