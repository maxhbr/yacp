{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import System.Exit
import Data.FileEmbed (embedFile)
import Data.Either
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as A
import qualified Data.Vector as V

import YACP.MyPrelude
import YACP.Model
import YACP.Collectors.OrtCollector
import YACP.Generators.Plantuml

identifierSpec = let
      purl1 = "pkg:pypi/Jinja2@2.11.2"
      purl2 = "pkg:maven/org.apache.xmlgraphics/batik-anim@1.9.1?packaging=sources"
  in
  describe "Identifier" $ do
    it ("parse purl1=" ++ purl1) $ do
      parsePURL purl1 `shouldBe` (PURL (Just "pkg") (Just "pypi") Nothing "Jinja2" (Just "2.11.2") Nothing Nothing)
    it ("parse purl2=" ++ purl2) $ do
      parsePURL purl2 `shouldBe` (PURL (Just "pkg") (Just "maven") (Just "org.apache.xmlgraphics") "batik-anim" (Just "1.9.1") (Just "?packaging=sources") Nothing)
    it ("test show of purl2=" ++ purl2) $ do
      show (parsePURL purl2) `shouldBe` purl2

ortFileBS :: B.ByteString
ortFileBS = B.fromStrict $(embedFile "test/data/analyzer-result.json")

ortSpec = let
    ortResult = A.eitherDecode ortFileBS :: Either String OrtFile
    potentialError = case ortResult of
      Right _ -> Nothing
      Left err -> Just err
  in do
  describe "OrtCollector" $ do
    it "parsing should not contain error" $ do
      potentialError `shouldBe` Nothing
    it "parsing is successfull" $ do
      isRight ortResult `shouldBe` True
    case ortResult of
      Right ortResult' -> do
        runIO $ print ortResult'
        it "expected number of project" $ do
          (length ((_or_projects . _of_Analyzer) ortResult')) `shouldBe` 3
        it "expected number of packagges" $ do
          (length ((_or_packages . _of_Analyzer) ortResult')) `shouldBe` 53
      _ -> return ()

runSpec = let
  yacp = do
    parseOrtBS ortFileBS
  in do
  describe "YACP" $ do
    (_, result) <- runIO $ runYACP yacp
    runIO $ print result

    case _getComponents result of
      Components cs -> do
        it "run is successfull and contains components" $ do
          V.length cs `shouldBe` 59
    case _getRelations result of
      Relations rs -> do
        it "run is successfull and contains relations" $ do
          V.length rs `shouldBe` 96

    runIO $ writePlantuml' result


main :: IO ()
main = hspec $ do
  identifierSpec
  ortSpec
  runSpec
