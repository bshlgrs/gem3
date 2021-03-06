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
import Data.List (find, isPrefixOf, intercalate)
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
  extendPath path2 = do
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
      PathRef path -> pure $ path
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
  eval path = do
    stuff <- get
    dim <- getDim (PathRef path)
    let equations = _equations stuff
    let relations = _relations stuff
    let relationEquations = S.fromList $ concat $ traverse (makeAllEquations (S.toList equations)) (zip relations [0..])

    case tryToSolve (relationEquations <> equations) path of
      Left x -> boom x
      Right x -> pure x

makeAllEquations :: [Value] -> (Path, RelationId) -> [Value]
makeAllEquations values (path, relationId) = catMaybes $ map tryToMakeEquation values
  where
    tryToMakeEquation :: Value -> Maybe Value
    tryToMakeEquation val = removeAllPrefixes val

    removeAllPrefixes :: Value -> Maybe Value
    removeAllPrefixes (CasExpr k factors) = CasExpr k . M.fromList <$>
                                traverse removePrefix (M.toList factors)

    removePrefix :: (VariableRef, Number) -> Maybe (VariableRef, Number)
    removePrefix (ref, n) = case ref of
      PathRef path2
        | path `isPrefixOf` path2 -> Just (RelationVarRef relationId (drop (length path) path2), n)
        | otherwise -> Nothing
      _ -> Nothing
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


tryToSolve :: S.Set Value -> Path -> Either String Number
tryToSolve equations var = -- error $ show var <> " " <> show equations
  maybe (Left "couldn't solve") Right
    (searchForSolution equations (PathRef var))
    -- error $ "Solving for " <> show var <> ":\n\n" <>
    --                                 intercalate "\n" (map show $ S.toList equations)


