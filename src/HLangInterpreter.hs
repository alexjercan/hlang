module HLangInterpreter where

import           HLangParser (Declaration (..), Expression (..), Factor (..),
                              Instruction (..), Literal (..), Program (..),
                              Statement (..), Term (..))

evalAddition :: Literal -> Literal -> Literal
evalAddition (Integer a) (Integer b) = Integer (a + b)
evalAddition _ _                     = undefined

evalDifference :: Literal -> Literal -> Literal
evalDifference (Integer a) (Integer b) = Integer (a - b)
evalDifference _ _                     = undefined

evalMultiplication :: Literal -> Literal -> Literal
evalMultiplication (Integer a) (Integer b) = Integer (a * b)
evalMultiplication _ _                     = undefined

evalDivision :: Literal -> Literal -> Literal
evalDivision (Integer a) (Integer b) = Integer (a `div` b)
evalDivision _ _                     = undefined

evalEqual :: Literal -> Literal -> Literal
evalEqual (Integer a) (Integer b) = Boolean $ a == b
evalEqual (Boolean a) (Boolean b) = Boolean $ a == b
evalEqual _ _                     = undefined

evalLessThan :: Literal -> Literal -> Literal
evalLessThan (Integer a) (Integer b) = Boolean $ a < b
evalLessThan _ _                     = undefined

evalLessThanEqual :: Literal -> Literal -> Literal
evalLessThanEqual (Integer a) (Integer b) = Boolean $ a <= b
evalLessThanEqual _ _                     = undefined

evalFactor :: Factor -> Literal
evalFactor (Atom atom)         = undefined
evalFactor (Literal literal)   = literal
evalFactor (Parenthesis instr) = evalInstruction instr

evalIfStatement :: Instruction -> Instruction -> Instruction -> Literal
evalIfStatement condition thenInstr elseInstr = case evalInstruction condition of
  Boolean b -> if b then evalInstruction thenInstr else evalInstruction elseInstr
  Integer x -> undefined

evalStatement :: Statement -> Literal
evalStatement (IfStatement condition thenInstr elseInstr) = evalIfStatement condition thenInstr elseInstr
evalStatement (FunctionCall factor stmt)                  = undefined
evalStatement (Factor factor)                             = evalFactor factor

evalTerm :: Term -> Literal
evalTerm (Multiplication statement term) = evalStatement statement `evalMultiplication` evalTerm term
evalTerm (Division statement term) = evalStatement statement `evalDivision` evalTerm term
evalTerm (Statement statement) = evalStatement statement

evalExpression :: Expression -> Literal
evalExpression (Addition term expr) = evalTerm term `evalAddition` evalExpression expr
evalExpression (Difference term expr) = evalTerm term `evalDifference` evalExpression expr
evalExpression (Term term) = evalTerm term

evalInstruction :: Instruction -> Literal
evalInstruction (Equal lhs rhs)         = evalExpression lhs `evalEqual` evalExpression rhs
evalInstruction (LessThan lhs rhs)      = evalExpression lhs `evalLessThan` evalExpression rhs
evalInstruction (LessThanEqual lhs rhs) = evalExpression lhs `evalLessThanEqual` evalExpression rhs
evalInstruction (Expression expr)       = evalExpression expr

evalDeclaration :: Declaration -> a
evalDeclaration (FunctionDeclaration func arg instr) = undefined

evalProgram :: Program -> Literal
evalProgram (Program decls instr) = evalInstruction instr
