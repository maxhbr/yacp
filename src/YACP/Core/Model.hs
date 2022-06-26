module YACP.Core.Model
  ( module X
  , idFromPurl
  , refinePurl
  , flexibilizePURL
  , nameAndVersion
  ) where

import           YACP.Core.MyPrelude

import           YACP.Core.Model.Identifier    as X
import           YACP.Core.Model.Relation      as X
import           YACP.Core.Model.Statement     as X

import           PURL.PURL                     as X

import qualified System.FilePath               as FP

idFromPurl :: String -> Identifier
idFromPurl uriStr = case parsePURL uriStr of
  Just purl -> PurlIdentifier purl
  Nothing   -> Identifier uriStr

refinePurl :: PURL -> PURL
refinePurl =
  let
    guessType :: String -> Maybe (PURL_Type, String)
    guessType ('g' : 'o' : ':' : rst) = Just (PURL_TypeGolang, rst)
    guessType str                     = case FP.splitPath str of
      "go" : rst -> Just (PURL_TypeGolang, FP.joinPath rst)
      fst  : rst -> case parsePURL_Type fst of
        PURL_Type _ -> Nothing
        ty'         -> Just (ty', FP.joinPath rst)
      [] -> Nothing
    applyGuessType :: PURL -> PURL
    applyGuessType (p@PURL { _PURL_namespace = Nothing, _PURL_name = name }) =
      case guessType name of
        Just (ty, name') -> p { _PURL_type = Just ty, _PURL_name = name' }
        Nothing          -> p
    applyGuessType (p@PURL { _PURL_namespace = Just namespace }) =
      case guessType namespace of
        Just (ty, namespace') ->
          p { _PURL_type = Just ty, _PURL_namespace = Just namespace' }
        Nothing -> p
    guessNamespaceFromName :: String -> Maybe (String, String)
    guessNamespaceFromName name = case FP.splitFileName name of
      ("./"      , _    ) -> Nothing
      (namespace', name') -> Just (dropTrailingPathSeparator namespace', name')
    applyGuessNamespaceFromName :: PURL -> PURL
    applyGuessNamespaceFromName (p@PURL { _PURL_namespace = Nothing, _PURL_name = name })
      = case guessNamespaceFromName name of
        Just (namespace, name') ->
          p { _PURL_namespace = Just namespace, _PURL_name = name' }
        Nothing -> p
  in
    applyGuessNamespaceFromName . applyGuessType

flexibilizePURL :: PURL -> Identifier
flexibilizePURL p =
  let refinedPurl = refinePurl p
      purlNoV     = case refinedPurl of
        p'@(PURL { _PURL_version = Just ('v' : vrst) }) ->
          [p' { _PURL_version = Just vrst }]
        p'@(PURL { _PURL_version = Just ('=' : vrst) }) ->
          [p' { _PURL_version = Just vrst }]
        _ -> []
  in  (mconcat . map PurlIdentifier) ([refinedPurl, p] ++ purlNoV)

nameAndVersion :: String -> String -> Identifier
nameAndVersion name version =
  let rawIdentifier = Identifier (name ++ "@" ++ version)
      rawPurl       = PURL (Just "pkg")
                           (Just PURL_TypeGeneric)
                           Nothing
                           name
                           (Just version)
                           Nothing
                           Nothing
  in  flexibilizePURL rawPurl <> rawIdentifier
