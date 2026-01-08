import Test.Hspec

import qualified MainSpec
import qualified ParseToASTSpec


main :: IO ()
main = hspec $ do
   MainSpec.spec
   ParseToASTSpec.spec