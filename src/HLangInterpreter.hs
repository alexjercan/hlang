module HLangInterpreter where

import           Control.Monad (join, liftM2)
import           HLangGrammar  (Atom, Declaration (..), Expression (..),
                                Factor (..), Instruction (..), Literal (..),
                                Program (..), Statement (..), Term (..))

data Binding = Binding Atom Literal deriving (Show, Eq)

data Context = Context [Declaration] [Binding] deriving (Show, Eq)

resolveBinding :: Atom -> [Binding] -> Maybe Literal
resolveBinding _ [] = Nothing
resolveBinding x ((Binding atom literal):rest)
  | atom == x = Just literal
  | otherwise = resolveBinding x rest

resolveDeclaration :: Atom -> [Declaration] -> Maybe (Atom, Instruction)
resolveDeclaration _ [] = Nothing
resolveDeclaration x ((FunctionDeclaration atom arg instr):rest)
  | atom == x = Just (arg, instr)
  | otherwise = resolveDeclaration x rest

evalAddition :: Literal -> Literal -> Maybe Literal
evalAddition (Integer a) (Integer b) = Just $ Integer (a + b)
evalAddition (Boolean a) (Boolean b) = Just $ Boolean (a || b)
evalAddition _ _                     = Nothing

evalDifference :: Literal -> Literal -> Maybe Literal
evalDifference (Integer a) (Integer b) = Just $ Integer (a - b)
evalDifference _ _                     = Nothing

evalMultiplication :: Literal -> Literal -> Maybe Literal
evalMultiplication (Integer a) (Integer b) = Just $ Integer (a * b)
evalMultiplication (Boolean a) (Boolean b) = Just $ Boolean (a && b)
evalMultiplication _ _                     = Nothing

evalDivision :: Literal -> Literal -> Maybe Literal
evalDivision (Integer a) (Integer b) = Just $ Integer (a `div` b)
evalDivision _ _                     = Nothing

evalEqual :: Literal -> Literal -> Maybe Literal
evalEqual (Integer a) (Integer b) = Just $ Boolean $ a == b
evalEqual (Boolean a) (Boolean b) = Just $ Boolean $ a == b
evalEqual _ _                     = Nothing

evalLessThan :: Literal -> Literal -> Maybe Literal
evalLessThan (Integer a) (Integer b) = Just $ Boolean $ a < b
evalLessThan _ _                     = Nothing

evalLessThanEqual :: Literal -> Literal -> Maybe Literal
evalLessThanEqual (Integer a) (Integer b) = Just $ Boolean $ a <= b
evalLessThanEqual _ _                     = Nothing

evalFactor :: Context -> Factor -> Maybe Literal
evalFactor (Context _ bindings) (Atom atom) = resolveBinding atom bindings
evalFactor _ (Literal literal)              = Just literal
evalFactor ctx (Parenthesis instr)          = evalInstruction ctx instr

evalIfStatement :: Context -> Instruction -> Instruction -> Instruction -> Maybe Literal
evalIfStatement ctx condition thenInstr elseInstr = case evalInstruction ctx condition of
  Just lit -> case lit of
    Boolean b -> if b then evalInstruction ctx thenInstr else evalInstruction ctx elseInstr
    Integer x -> Nothing
  Nothing -> Nothing

evalStatement :: Context -> Statement -> Maybe Literal
evalStatement ctx (IfStatement condition thenInstr elseInstr) = evalIfStatement ctx condition thenInstr elseInstr
evalStatement ctx@(Context decls bindings) (FunctionCall atom stmt) = case resolveDeclaration atom decls of
  Just (arg, instr) -> case evalStatement ctx stmt of
    Just value -> evalInstruction (Context decls (Binding arg value:bindings)) instr
    Nothing -> Nothing
  Nothing -> Nothing
evalStatement ctx (Factor factor)                             = evalFactor ctx factor

evalTerm :: Context -> Term -> Maybe Literal
evalTerm ctx (Multiplication statement term) = join $ liftM2 evalMultiplication (evalStatement ctx statement) (evalTerm ctx term)
evalTerm ctx (Division statement term)       = join $ liftM2 evalDivision (evalStatement ctx statement) (evalTerm ctx term)
evalTerm ctx (Statement statement)           = evalStatement ctx statement

evalExpression :: Context -> Expression -> Maybe Literal
evalExpression ctx (Addition term expr)   = join $ liftM2 evalAddition (evalTerm ctx term) (evalExpression ctx expr)
evalExpression ctx (Difference term expr) = join $ liftM2 evalDifference (evalTerm ctx term) (evalExpression ctx expr)
evalExpression ctx (Term term)            = evalTerm ctx term

evalInstruction :: Context -> Instruction -> Maybe Literal
evalInstruction ctx (Equal lhs rhs)         = join $ liftM2 evalEqual (evalExpression ctx lhs) (evalExpression ctx rhs)
evalInstruction ctx (LessThan lhs rhs)      = join $ liftM2 evalLessThan (evalExpression ctx lhs) (evalExpression ctx rhs)
evalInstruction ctx (LessThanEqual lhs rhs) = join $ liftM2 evalLessThanEqual (evalExpression ctx lhs) (evalExpression ctx rhs)
evalInstruction ctx (Expression expr)       = evalExpression ctx expr

evalDeclaration :: [Declaration] -> Context
evalDeclaration decls = Context decls []

evalProgram :: Program -> Maybe Literal
evalProgram (Program decls instr) = evalInstruction (evalDeclaration decls) instr
