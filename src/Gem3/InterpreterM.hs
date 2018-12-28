module Gem3.InterpreterM where
import Gem3.Data

class Monad m => InterpreterM m where
  boom :: InterpreterError -> m a
  extendPath :: Path -> m Path
  declVal :: Path -> Value -> Dimension -> m ()
  declType :: Path -> Dimension -> m ()
  setEqual :: Value -> Value -> m ()
  declareModule :: String -> m () -> m ()
  getDim :: VariableRef -> m Dimension
  makeRelation :: Path -> m Int
  printString :: String -> m ()
  getRelationId :: Path -> m RelationId
  eval :: Path -> m Number
