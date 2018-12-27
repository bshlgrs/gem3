module Gem3.Interpreter where

import Gem3.Data
import Gem3.InterpreterM
import Control.Monad.RWS.Lazy
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import qualified Data.Set as S
import Control.Lens.TH
import Control.Lens
import Gem3.Cas
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (catMaybes)



data InterpreterState = InterpreterState {
  _equations :: S.Set Value,
  _variableDimensions :: M.Map Path Dimension,
  _relations :: [Path],
  _relationReferences :: M.Map Path RelationId
} deriving (Show, Eq)

emptyInterpreterState = InterpreterState S.empty M.empty [] M.empty

$(makeLenses ''InterpreterState)
-- RWS, Either
type Interpreter = RWST Path [String] InterpreterState (Either InterpreterError)

instance InterpreterM Interpreter where
  boom x = do
    path <- ask
    stuff <- get
    lift $ Left $ "At path " <> show path <> ": " <> x <> "\n\n" <> show stuff
  extendName path2 = do
    path <- ask
    pure $ path ++ path2
  declVal name val dim = do
    -- TODO: check reasonableness in this call
    equations %= S.insert (pureVar (PathRef name) / val)
    variableDimensions %= M.insert name dim
  declType name dim = variableDimensions %= M.insert name dim
  setEqual x y = equations %= S.insert (x / y)
  declareModule name stuff = local (++ [name]) stuff
  getDim varRef = do
    pathToQuery <- case varRef of
      PathRef path -> extendName path
      RelationVarRef id path -> do
        relations <- _relations <$> get
        let parentPath = relations !! id
        pure $ parentPath <> path

    varDims <- _variableDimensions <$> get
    case M.lookup pathToQuery varDims of
      Nothing -> boom $ "You tried to get dimensions of " <> show varRef <>
                           " (which resolved to " <> show pathToQuery <>
                                     "), which is undefined"
      Just x -> pure x
  makeRelation name = do
    relations %= (++ [name])
    stuff <- get
    -- todo: fancy lenses here too
    pure $ length (_relations stuff) - 1
  printString s = tell [s]
  -- Here is where all the work is.
  getRelationId name = do
    stuff <- get
    -- todo: fancy lenses
    case M.lookup name (_relationReferences stuff) of
      Nothing -> boom $ "You tried to refer to the alleged relation " <> show name <>
                                        ", which is undefined"
      Just x -> pure x
  solve path = do
    stuff <- get
    let equations = S.toList $ _equations stuff
    let relations = _relations stuff
    relationEquations <- traverse (makeAllEquations equations) (zip relations [0..])

    case tryToSolve (concat relationEquations <> equations) path of
      Left x -> boom x
      Right x -> pure x

makeAllEquations :: [Value] -> (Path, RelationId) -> Interpreter [Value]
makeAllEquations values (path, relationId) =
      catMaybes <$> traverse tryToMakeEquation values
  where
    tryToMakeEquation :: Value -> Interpreter (Maybe Value)
    tryToMakeEquation val = undefined
{-
filter the equation based on whether all its variables are inside the path

if so, rewrite all of them to instead be RelationVarRefs.

Eg if we had

path = ["KEDef"]
relationId = 5
tryToMakeEquation ([KEDef.Energy, KEDef.Mass, KEDef.Velocity])

the result would be

[RelationVarRef 5 Energy, RelationVarRef 5 Mass, RelationVarRef 5 Velocity]

-}


tryToSolve :: [Value] -> Path -> Either String Value
tryToSolve equations var = undefined
