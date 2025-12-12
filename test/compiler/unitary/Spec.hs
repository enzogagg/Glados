import Test.Hspec

import qualified MainSpec
import qualified ParseToExprSpec
import qualified ParseValueSpec
import qualified TranslateASTSpec

main :: IO ()
main = hspec $ do
    MainSpec.spec
    ParseToExprSpec.spec
    ParseValueSpec.spec
    TranslateASTSpec.spec
