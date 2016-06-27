import Data.Monoid ((<>))
import System.Environment (withArgs)

import Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.Golden (goldenVsFile)

import Sjd (runMain)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [goldenTestsWrapper]

goldenTestsWrapper :: TestTree
goldenTestsWrapper = testGroup
  "GoldenTestsWrapper"
  [ withResource
      (withArgs ["test/customer.sjd"] runMain)
      (\_ -> return ())
      (const goldenTests)]

goldenTests :: TestTree
goldenTests = testGroup "GoldenTests" [domainClasses, builderClasses]

domainClasses :: TestTree
domainClasses = testGroup "DomainClasses" $
                  map goldenJavaTest ["Customer"
                                    , "Item"
                                    , "Product"
                                    , "Specials"
                                    , "SubSpecials"]

builderClasses :: TestTree
builderClasses = testGroup "BuilderClasses" $
                  map goldenJavaTest ["CustomerBuilder"
                                    , "ItemBuilder"
                                    , "ProductBuilder"
                                    , "SpecialsBuilder"
                                    , "SubSpecialsBuilder"]

goldenJavaTest :: String -> TestTree
goldenJavaTest className = goldenVsFile
                             className
                             ("test/golden/" <> className <> ".java")
                             ("src/main/java/" <> className <> ".java")
                             (return ())
