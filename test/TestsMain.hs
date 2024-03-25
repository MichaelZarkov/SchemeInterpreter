
import Test.Hspec ( hspec, parallel )

import RacketEvalSpec ( racketEvalSpec )
import RacketParserSpec ( racketParserSpec )

main :: IO ()
main = hspec $ parallel $ do
  racketEvalSpec
  racketParserSpec


