import Data.Monoid ((<>))
import System.Environment (withArgs)

import Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.Golden (goldenVsFile)

import Sjd (runMain)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [goldenTests]

goldenTests :: TestTree
goldenTests = testGroup
  "GoldenTests"
  [withResource
    (withArgs ["test/customer.sjd"] runMain)
    (\_ -> return ())
    (const domainClasses)]

domainClasses :: TestTree
domainClasses = testGroup "DomainClasses" $
                  map goldenJavaTest ["Customer"
                                    , "CustomerBuilder"
                                    , "Item"
                                    , "ItemBuilder"
                                    , "Product"
                                    , "ProductBuilder"
                                    , "Specials"
                                    , "SpecialsBuilder"
                                    , "SubSpecials"
                                    , "SubSpecialsBuilder"]

goldenJavaTest :: String -> TestTree
goldenJavaTest className = goldenVsFile
                             className
                             ("test/golden/" <> className <> ".java")
                             ("src/main/java/" <> className <> ".java")
                             (return ())
