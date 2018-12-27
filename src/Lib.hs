module Lib where
import Data.Map (Map, fromList)
import qualified Data.Map as M
import Data.Group
import Gem3.Data
import Gem3.Interpreter
import Gem3.Interpret
import Control.Monad.RWS

kineticEnergyDefMod = Module [
    DeclType "mass" kgDim,
    DeclType "velocity" (mDim <> invert sDim),
    DeclType "energy" joule,
    SetEqual (simpleVar "energy") (simpleVar "mass" * simpleVar "velocity" `numPow` 2)
  ]

kineticEnergyUseExample = [
    DeclType "endVelocity" (mDim <> pow sDim (-2)),
    DeclType "ballMass" kgDim,
    DeclVal "slideHeight" (Num (fromInteger 4) mDim),
    DeclVal "endEnergy"
      (FieldExpr (ModuleCallExpr ["keDef"] (fromList [(["mass"], simpleVar "ballMass"),
          (["velocity"], simpleVar "endVelocity")])) "energy"),
    DeclVal "startEnergy"
      (simpleVar "ballMass" * (Var $ PathRef ["normal", "gravity"]) * simpleVar "slideHeight"),
    SetEqual (simpleVar "startEnergy") (simpleVar "endEnergy")
  ]

gravityDecl = (Left $ ModuleStmt "normal" (Module [DeclVal "gravity" (Num (fromInteger 10)
                                        (mDim <> pow sDim (-2)))]))
initialProgram = (Left $ ModuleStmt "keDef" kineticEnergyDefMod) :
                 gravityDecl :
                 (map Left kineticEnergyUseExample) ++
                 [Right $ SolveCommand ["endVelocity"],
                  Right $ PrintExprCommand (simpleVar "endVelocity")]

runProgram :: Program -> Either InterpreterError ((), InterpreterState, [String])
runProgram prog =
  runRWST (interpretProgram prog :: Interpreter ()) [] emptyInterpreterState

main = case runProgram initialProgram of
  Left x -> putStrLn x
  Right x -> print x
