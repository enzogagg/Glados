import Test.Hspec

import qualified MainSpec
import qualified ParseToASTSpec
import qualified AstToBinSpec


main :: IO ()
main = hspec $ do
   MainSpec.spec
   ParseToASTSpec.spec
   AstToBinSpec.spec