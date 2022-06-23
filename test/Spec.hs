{-# LANGUAGE DeriveGeneric #-}
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
            `shouldBe` (PURL
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
            `shouldBe` (PURL
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

main :: IO ()
main = hspec $ do
  identifierSpec
