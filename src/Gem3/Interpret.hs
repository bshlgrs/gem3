module Gem3.Interpret where
import Gem3.Data
import Control.Monad
import qualified Gem3.Cas as Cas
import qualified Data.Map as M
import Gem3.InterpreterM

interpretStmt :: InterpreterM m => Stmt -> m ()
interpretStmt = \case
  DeclVal str expr -> do
    (val, dim) <- interpretExpr expr
    fullName <- extendName [str]
    declVal fullName val dim
  DeclType name dimension -> do
    flip declType dimension =<< extendName [name]
  SetEqual l r -> do
    (lVal, lDim) <- interpretExpr l
    (rVal, rDim) <- interpretExpr r
    if lDim /= rDim then boom "Dimensions don't match" else pure ()
    setEqual lVal rVal
  ModuleStmt name (Module stmts) -> do
    declareModule name (traverse interpretStmt stmts >> pure ())

interpretExpr :: InterpreterM m => Expr -> m (Value, Dimension)
interpretExpr = \case
  Num number dimension -> pure (Cas.pureNum number, dimension)
  Var varRef -> do
    dim <- getDim varRef
    pure (Cas.pureVar varRef, dim)
  FuncCall ["*"] [x, y] -> do
    (xVal, xDim) <- interpretExpr x
    (yVal, yDim) <- interpretExpr y
    pure (xVal * yVal, xDim <> yDim)
  FuncCall other _ -> error $ "You called the function " <> show other <>
                                ", but only multiplcation is defined right now"
  FieldExpr modExpr fieldName -> do
    relationId <- interpretModuleExpr modExpr
    let varRef = RelationVarRef relationId [fieldName]
    dim <- getDim varRef
    pure (Cas.pureVar $ varRef, dim)

interpretModuleExpr :: InterpreterM m => ModuleExpr -> m RelationId
interpretModuleExpr = \case
  ModuleCallExpr name givens -> do
    let givensList = M.toList givens
    interpretedGivens <- traverse (\(x, y) -> (x,) . fst <$> interpretExpr y) givensList
    newRelationId <- makeRelation name
    -- traverse ()
    let handleEq (name, val) = setEqual (Cas.pureVar $ PathRef name) val
    traverse handleEq interpretedGivens
    pure newRelationId
  ModuleNameExpr name -> getRelationId name

interpretCommand :: InterpreterM m => Command -> m ()
interpretCommand = \case
  SolveCommand name -> do
    solution <- solve name
    printString $ show solution
  PrintStrCommand str -> printString str
  PrintExprCommand expr -> do
    value <- interpretExpr expr
    printString $ show value
    pure ()

interpretProgram :: InterpreterM m => [Either Stmt Command] -> m ()
interpretProgram p = traverse (either interpretStmt interpretCommand) p >> pure ()
