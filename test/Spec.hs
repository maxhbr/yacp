{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import           Control.Exception              ( evaluate )
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Encode.Pretty      as A
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as B
import           Data.Either
import           Data.FileEmbed                 ( embedFile )
import qualified Data.Graph.Inductive.Graph    as G
import           Data.List                      ( isPrefixOf
                                                , tails
                                                )
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Vector                   as V
import qualified Data.Yaml                     as Y
import qualified PURL.PURL                     as PURL
import           System.Directory               ( doesFileExist )
import           System.Exit
import           System.IO.Temp                 ( withSystemTempDirectory )
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import           Test.Hspec
import           Test.QuickCheck

import           YACP

identifierSpec =
  let purl1 = "pkg:pypi/Jinja2@2.11.2"
      purl2 =
        "pkg:maven/org.apache.xmlgraphics/batik-anim@1.9.1?packaging=sources"
  in  describe "Identifier" $ do
        it ("parse purl1=" ++ purl1) $ do
          parsePURL purl1
            `shouldBe` (PurlIdentifier
                         (PURL.PURL (Just "pkg")
                                    (Just (PURL.parsePURL_Type "pypi"))
                                    Nothing
                                    "Jinja2"
                                    (Just "2.11.2")
                                    Nothing
                                    Nothing
                         )
                       )
        it ("parse purl2=" ++ purl2) $ do
          parsePURL purl2
            `shouldBe` (PurlIdentifier
                         (PURL.PURL (Just "pkg")
                                    (Just (PURL.parsePURL_Type "maven"))
                                    (Just "org.apache.xmlgraphics")
                                    "batik-anim"
                                    (Just "1.9.1")
                                    (Just "?packaging=sources")
                                    Nothing
                         )
                       )
        it ("test show of purl2=" ++ purl2) $ do
          show (parsePURL purl2) `shouldBe` purl2

componentDetectionSpec =
  let componentDetectionFileBS :: B.ByteString
      componentDetectionFileBS =
        B.fromStrict
          $(embedFile "test/data/component-detection/component-detection.json")
  in  describe "ComponentDetectionReader" $ do
        let parsed = parseComponentDetectionBS componentDetectionFileBS
        it ("parsing should succeed") $ do
          when (isLeft parsed) $ parsed `shouldBe` (Left (YACPParsingIssue ""))
          isRight parsed `shouldBe` True

fosslightDependencyReportSpec =
  let fosslightFileBS :: B.ByteString
      fosslightFileBS =
        B.fromStrict
          $(embedFile "test/data/fosslight/fosslight_dependency-Report_SRC.csv")
  in describe "FosslightDependencyReportReader" $ do
        let parsed = parseFosslightDepRepBS fosslightFileBS
        it ("parsing should succeed") $ do
          when (isLeft parsed) $ parsed `shouldBe` (Left (YACPParsingIssue ""))
          isRight parsed `shouldBe` True

itDependsReportSpec =
  let itDependsFileBS :: B.ByteString
      itDependsFileBS =
        B.fromStrict
          $(embedFile "test/data/it-depends/it-depends.json")
  in describe "ItDependsReportReader" $ do
        let parsed = parseItDependsBS itDependsFileBS
        it ("parsing should succeed") $ do
          when (isLeft parsed) $ parsed `shouldBe` (Left (YACPParsingIssue ""))
          isRight parsed `shouldBe` True

main :: IO ()
main = hspec $ do
  identifierSpec
  componentDetectionSpec
  fosslightDependencyReportSpec
  itDependsReportSpec
