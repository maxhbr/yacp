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
import Data.List (tails, isPrefixOf)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Vector as V
import qualified Data.Map as Map
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (doesFileExist)
import qualified Data.Graph.Inductive.Graph as G

import YACP
import YACP.HHC.HHC

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
        it "expected number of project" $ do
          (length ((_or_projects . _of_Analyzer) ortResult')) `shouldBe` 3
        it "expected number of packages" $ do
          (length ((_or_packages . _of_Analyzer) ortResult')) `shouldBe` 53
      _ -> return ()

spdxFileBS :: B.ByteString
spdxFileBS = B.fromStrict $(embedFile "data/spdx-spdx-spec/examples/SPDXJSONExample-v2.2.spdx.json")

spdxSpec = let
    spdxResult = A.eitherDecode spdxFileBS :: Either String SPDXDocument
    potentialError = case spdxResult of
      Right _ -> Nothing
      Left err -> Just err
  in do
  describe "SpdxCollector" $ do
    it "parsing should not contain error" $ do
      potentialError `shouldBe` Nothing
    it "parsing is successfull" $ do
      isRight spdxResult `shouldBe` True
    case spdxResult of
      Right spdxResult' -> do
        runIO (print spdxResult')
      Left err -> runIO (print err)

scancodeFileBS :: B.ByteString
scancodeFileBS = B.fromStrict $(embedFile "test/data/bat.scancode.pp.json")

scancodeSpec = let
    scancodeResult = A.eitherDecode scancodeFileBS :: Either String ScancodeFile
    potentialError = case scancodeResult of
      Right _ -> Nothing
      Left err -> Just err
  in do
  describe "ScancodeCollector" $ do
    it "parsing should not contain error" $ do
      potentialError `shouldBe` Nothing
    it "parsing is successfull" $ do
      isRight scancodeResult `shouldBe` True


graphSpec = do
  describe "Graph" $ let
    yacp = do
      parseOrtBS ortFileBS
      computeGraph
    in do
    (graph, result) <- runIO $ runYACP yacp
    it "num of edges should be OK" $ do
      length (G.edges graph) `shouldBe` 226
    it "num of nodes should be OK" $ do
      length (G.nodes graph) `shouldBe` 197
  describe "GraphWithDepths" $ let
    yacp= do
      addRelation (Relation "a" DEPENDENCY_OF "b")
      addRelation (Relation "a" DEPENDENCY_OF "c")
      addRelation (Relation "c" DEPENDENCY_OF "b")
      addRelation (Relation "b" DEPENDENCY_OF "d")
      addRelation (Relation "A" DEPENDENCY_OF "B")
      addRoot "d"
      addRoot "B"
      addComponent (Component "-1" Nothing mempty mempty mempty)
      addComponent (Component ("b" <> "b_2") Nothing mempty mempty mempty)
      ppGraph
      computeGraphWithDepths
    in do
    (graph, result) <- runIO $ runYACP yacp

    it "hasCorrectDepths" $ let
      nodes = G.labNodes graph
      depths = List.sort $ map (\(_,(_,d)) -> d) nodes
      in depths `shouldBe` [-1,0,0,1,1,2,2]


plantumlSpec = let
  in do
  describe "plantuml" $ do
    svgExists <- runIO $ withSystemTempDirectory "yacp"
        (\td -> let
            yacp = do
              parseOrtBS ortFileBS
              writePlantumlFile (td </> "out.puml")
            in do
            runYACP yacp
            let svg = (td </> "out.svg")
            doesFileExist svg)
    it "svg should be created" $ do
      svgExists `shouldBe` True

    return ()

hhcSpec =do
  describe "HHC Model" $ let 
      exmpResourses = [ "root" </> "subfolder" </> "file1"
                      ,  "root" </> "subfolder" </> "file2"
                      , "root" </> "file3"
                      ]
      parsedResources = (HHC_Resources
                          (Map.singleton "root" (HHC_Resources
                                                  (Map.singleton "subfolder" (HHC_Resources
                                                                          Map.empty
                                                                          (Set.fromList [ "file1"
                                                                                        , "file2"])))
                                                  (Set.singleton "file3")))
                          (Set.empty))
      serializedResources = B.concat
        [ "{"
        ,   "\"root\":{"
        ,     "\"subfolder\":{"
        ,       "\"file1\":1,"
        ,       "\"file2\":1"
        ,     "},"
        ,     "\"file3\":1"
        ,   "}"
        , "}"
        ]
      otherResources = HHC_Resources (Map.singleton "root" (HHC_Resources (Map.singleton "other" (HHC_Resources Map.empty (Set.singleton "file4"))) Set.empty)) (Set.singleton "file5")
      allResourcesSerialized = "{\"root\":{\"other\":{\"file4\":1},\"subfolder\":{\"file1\":1,\"file2\":1},\"file3\":1},\"file5\":1}"
    in do
      it "testFolderAndFileMerging" $ do
        (fpToResources FileType_File "path/to/file") <> (fpToResources FileType_Folder "path/other/dir") `shouldBe` 
          HHC_Resources ( Map.singleton "path" 
                                        (HHC_Resources ( Map.fromList [("to", HHC_Resources Map.empty (Set.singleton "file"))
                                                                      ,("other", HHC_Resources (Map.singleton "dir" mempty) Set.empty)]
                                                      )
                                                      Set.empty)
                        )
                        Set.empty
        countFiles ((fpToResources FileType_File "path/to/file") <> (fpToResources FileType_Folder "path/other/dir")) `shouldBe` 1

      it "testFileListParsing" $ do
        fpsToResources exmpResourses `shouldBe` parsedResources
      it "testFileListSerialization" $ do
        A.encode parsedResources `shouldBe` serializedResources
      it "testMergeAndFileListSerialization" $ do
        A.encode (parsedResources <> otherResources) `shouldBe` allResourcesSerialized

  describe "HHCWriter" $ let
      yacp = do
        parseOrtBS ortFileBS
        parseScancodeBS scancodeFileBS
        computeHHC
    in do
      (hhc@(HHC _ rs _ _ _), _) <- runIO $ runYACP yacp
      it "numOfFilesShouldMatch" $ do
        countFiles (resources hhc) `shouldBe` 503
      it "numOfFilesShouldMatch" $ do
        countFiles rs `shouldBe` (length . filter (isPrefixOf ": 1") . tails . C.unpack . B.toStrict . A.encodePretty $ rs)
      it "numOfExternalAttributionsShouldMatch" $ do
        length (externalAttributions hhc) `shouldBe` 197
      it "numOfResourcesToAttributionsShouldMatch" $ do
        length (resourcesToAttributions hhc) `shouldBe` 41
      runIO (writeHHCStats hhc)


runSpec = let
  yacp = do
    parseOrtBS ortFileBS
    parseScancodeBS scancodeFileBS
    parseSPDXBS spdxFileBS

    -- ppState

    -- ppGraph

    ppStats
  in do
  describe "YACP" $ do
    (_, result) <- runIO $ runYACP yacp
    case _getComponents result of
      Components cs -> do
        it "run is successfull and contains components" $ do
          V.length cs `shouldBe` 366
    case _getRelations result of
      Relations rs -> do
        it "run is successfull and contains relations" $ do
          V.length rs `shouldBe` 402

main :: IO ()
main = hspec $ do
  identifierSpec
  graphSpec
  ortSpec
  spdxSpec
  scancodeSpec
  plantumlSpec
  hhcSpec
  runSpec
