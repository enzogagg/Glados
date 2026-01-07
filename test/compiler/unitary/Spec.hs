import Test.Hspec

import qualified MainSpec
import qualified ParseToASTSpec
import qualified CompilerSpec
import qualified BytecodeGeneratorSpec

main :: IO ()
main = hspec $ do
    describe "MainSpec" MainSpec.spec
    describe "ParseToASTSpec" ParseToASTSpec.spec
    describe "CompilerSpec" CompilerSpec.spec
    describe "BytecodeGeneratorSpec" BytecodeGeneratorSpec.spec
