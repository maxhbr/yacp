{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module YACP.Reader.ScancodeReader
  ( readScancodeFile
  , readScancodeBS
  -- for testing:
  , parseScancodeBS
  ) where

import           YACP.Core

import qualified Control.Monad.State           as MTL
import qualified Data.Aeson                    as A
import qualified Data.Aeson.KeyMap             as A
import qualified Data.Aeson.Types              as A
import qualified Data.ByteString.Lazy          as B
import qualified Data.Map                      as Map
import qualified Data.Vector                   as V
import qualified System.IO                     as IO
import qualified Data.List                as List

import qualified Data.Text                as T

import Data.Monoid


data ScancodePackage =
  ScancodePackage
    { _scp_purl         :: Maybe PURL
    , _scp_licenses     :: MaybeLicenseExpression
    , _scp_copyright    :: Maybe String
    , _scp_dependencies :: [ScancodePackage]
    }
  deriving (Eq, Show)

instance A.FromJSON ScancodePackage where
  parseJSON =
    A.withObject "ScancodePackage" $ \v -> do
      purl <-
        v A..:? "purl" >>=
        (\case
           Just purl -> return $ parsePURL purl
           Nothing   -> return Nothing)
      dependencies <-
        (v A..:? "dependencies" >>=
         (\case
            Just dependencies -> mapM A.parseJSON dependencies
            Nothing           -> return [])) :: A.Parser [ScancodePackage]
      license <- fmap (fromMaybe mempty) $ v A..:? "license_expression"
      copyright <- v A..:? "copyright"
      return $ ScancodePackage purl license copyright dependencies

data ScancodeFileEntry =
  ScancodeFileEntry
    { _scfe_file       :: FilePath
    , _scfe_is_file    :: Bool
    , _scfe_license    :: MaybeLicenseExpression
    , _scfe_copyrights :: [String]
    , _scfe_packages   :: [ScancodePackage]
    }
  deriving (Eq, Show)

instance A.FromJSON ScancodeFileEntry where
  parseJSON =
    A.withObject "ScancodeFileEntry" $ \v -> do
      path <- v A..: "path"
      is_file <-
        (\case
           ("file" :: String) -> True
           _                  -> False) <$>
        v A..: "type"
    -- sha1 <- v `getHash` "sha1"
    -- md5 <- v `getHash` "md5"
    -- sha256 <- v `getHash` "sha256"
    -- sha512 <- v `getHash` "sha512"
    -- let idFromHashes = mconcat $ sha1 ++ md5 ++ sha256 ++ sha512
      let applyAll = appEndo . mconcat . map Endo
      licenseTransformator <-
        do licenseObjects <- (v A..: "licenses" :: A.Parser [A.Object])
           licenseNameTuples <-
             mapM
               (\v' -> do
                  key <- v' A..: "key" :: A.Parser T.Text
                  spdxkey <-
                    v' A..:? "spdx_license_key" :: A.Parser (Maybe T.Text)
                  return (key, spdxkey))
               licenseObjects
           (return .
            (\fun -> T.unpack . fun . T.pack) .
            applyAll .
            map (\(k1, Just s1) -> T.replace k1 s1) .
            List.sortBy
              (\(k1, _) -> \(k2, _) -> (T.length k1) `compare` (T.length k2)) .
            filter (\(_, spdxkey) -> isJust spdxkey))
             licenseNameTuples
      license <-
        v A..:? "license_expressions" >>=
        (return .
         (\case
            Just lics -> mconcat (map (fromString . licenseTransformator) lics)
            Nothing   -> MLicExp NOASSERTION))
      copyrights <-
        do listOfCopyrightObjects <-
             (v A..:? "copyrights" :: A.Parser (Maybe A.Array))
           case listOfCopyrightObjects of
             Just cos ->
               let getValueFromCopyrightObject =
                     A.withObject "CopyrightsEntry" $ \v' ->
                       v' A..: "value" :: A.Parser String
                in mapM getValueFromCopyrightObject (V.toList cos)
             Nothing -> return []
      packages <-
        (\case
           Just ps -> ps
           Nothing -> []) <$>
        v A..:? "packages"
      return (ScancodeFileEntry path is_file license copyrights packages)

data ScancodeFile =
  ScancodeFile
    { _scf_metadata :: A.Value
    , _scf_files    :: [ScancodeFileEntry]
    }
  deriving (Eq, Show)

instance A.FromJSON ScancodeFile where
  parseJSON =
    A.withObject "ScancodeFile" $ \v -> do
      ScancodeFile <$> v A..: "headers" <*> v A..: "files"

convertScancode :: ScancodeFile -> Statements
convertScancode (ScancodeFile _ files) = let
      convertScancodeFilePackages :: Statemental a => Identifier -> (Identifier -> a) -> ScancodePackage -> Statements
      convertScancodeFilePackages fileIdentifier fun scp = case (_scp_purl scp) of
        Just purl -> let
            packageIdentifier = PurlIdentifier purl
            packagesStatements = mconcat $ map (convertScancodeFilePackages packageIdentifier FoundDependent) (_scp_dependencies scp)
          in (Statements $ V.fromList [ Statement packageIdentifier (ComponentLicense (_scp_licenses scp))
                                     , Statement packageIdentifier (fun fileIdentifier)
                                     ]) <> packagesStatements
        Nothing -> Statements $ V.fromList [ Statement fileIdentifier (ComponentLicense (_scp_licenses scp)) ]
      convertScancodeFile :: ScancodeFileEntry -> Statements
      convertScancodeFile scfe = let
          fileIdentifier = AbsolutePathIdentifier (_scfe_file scfe)
          fileLicenses = DetectedLicenses [_scfe_license scfe]
          packagesStatements = mconcat $ map (convertScancodeFilePackages fileIdentifier FoundManifestFile) (_scfe_packages scfe)
        in (Statements $ V.fromList [Statement fileIdentifier fileLicenses]) <> packagesStatements
    in mconcat $ map convertScancodeFile files

parseScancodeBS :: B.ByteString -> Either YACPIssue ScancodeFile
parseScancodeBS bs = case A.eitherDecode bs of
  Right cd  -> Right cd
  Left  err -> Left (YACPParsingIssue err)

readScancodeBS :: Origin -> B.ByteString -> YACP (Maybe YACPIssue)
readScancodeBS o bs = case parseScancodeBS bs of
  Right file -> do
    let statements = setOrigin o $ convertScancode file
    addStatements statements
    return Nothing
  Left issue -> return (Just issue)

readScancodeFile :: FilePath -> YACP ()
readScancodeFile f = readBSFromFile (readScancodeBS (OriginToolReport "scancode" f)) f