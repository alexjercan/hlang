module Main where

import           HLang        (interpret)
import           HLangGrammar (Literal (..))
import           Test.HUnit   (Test (..), assertEqual, runTestTTAndExit)

tests :: [(String, String, Maybe Literal)]
tests = [ ("Test Addition", ">>> 1 + 1", Just $ Integer 2)
        , ("Test Expression", ">>> (1 + 2) * 9 + 8 * 5", Just $ Integer 67)
        , ("Test Equal", ">>> 1 + 1 == (2 + 2) * 2", Just $ Boolean False)
        , ("Test Less Than Equal", ">>> 1 + 1 <= (2 + 2) * 2", Just $ Boolean True)
        , ("Test Less Than", ">>> 1 + 1 < (2 + 2) * 2", Just $ Boolean True)
        , ("Test If Statement", ">>> if 1 == 1 then 0 else 2 + 2 fi", Just $ Integer 0)
        , ("Test If Statement", ">>> if 1 == 1 then if 2 == 2 then 0 else 1 fi else 2 + 2 fi", Just $ Integer 0)
        , ("Test Function Declaration", "let atom1 x => x + x end;\n>>> 1", Just $ Integer 1)
        , ("Test Function Call", "let atom1 x => x + x end;\n>>> $ atom1 1 + 1", Just $ Integer 3)
        , ("Test Function Call", "let atom1 x => x + x end;\n>>> $ atom1 if 1 == 1 then 1 else 2 fi", Just $ Integer 2)
        , ("Test Program", "let square x => if x == 0 then 0 else x * x fi end;\nlet func x => $ square x + $ square x end;\n>>> $ func 5", Just $ Integer 50)
        ]

mkTest :: (Eq a, Show a) => (t -> a) -> (String, t, a) -> Test
mkTest f (desc, a, b) = TestCase $ assertEqual desc b (f a)

mkTests :: (Eq a, Show a) => String -> (t -> a) -> [(String, t, a)] -> Test
mkTests label f xs = TestList $ map (TestLabel label . mkTest f) xs

main :: IO ()
main = runTestTTAndExit $ mkTests "Interpreter Tests" interpret tests
