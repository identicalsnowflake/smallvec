import Data.Vector.Small as SV
import Data.Vector.Generic as GV
import Test.Hspec
import Test.QuickCheck


main :: IO ()
main = hspec $ do
  describe "Small" $ do
    it "sanity checks" $ property \(xs :: [ (Int , Float) ]) ->
      toList (GV.fromList xs :: SV.Vector (Int , Float)) == xs

