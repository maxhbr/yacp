{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module YACP.Model.Relation
  ( RelationType (..)
  , Relation (..), normalizeRelation
  ) where

import YACP.MyPrelude

import YACP.Model.Identifier

-- see: https://spdx.github.io/spdx-spec/7-relationships-between-SPDX-elements/
data RelationType
  = DESCRIBES
    -- Is to be used when SPDXRef-DOCUMENT describes SPDXRef-A.
    -- An SPDX document WildFly.spdx describes package ‘WildFly’. Note this is a logical relationship to help organize related items within an SPDX document that is mandatory if more than one package or set of files (not in a package) is present.
  | DESCRIBED_BY
    -- Is to be used when SPDXRef-A is described by SPDXREF-Document.
    -- The package ‘WildFly’ is described by SPDX document WildFly.spdx.
  | CONTAINS
    -- Is to be used when SPDXRef-A contains SPDXRef-B.
    -- An ARCHIVE file bar.tgz contains a SOURCE file foo.c.
  | CONTAINED_BY
    -- Is to be used when SPDXRef-A is contained by SPDXRef-B.
    -- A SOURCE file foo.c is contained by ARCHIVE file bar.tgz
  | DEPENDS_ON
    -- Is to be used when SPDXRef-A depends on SPDXRef-B.
    -- Package A depends on the presence of package B in order to build and run
  | DEPENDENCY_OF
    -- Is to be used when SPDXRef-A is dependency of SPDXRef-B.
    -- A is explicitly stated as a dependency of B in a machine-readable file. Use when a package manager does not define scopes.
  | DEPENDENCY_MANIFEST_OF
    -- Is to be used when SPDXRef-A is a manifest file that lists a set of dependencies for SPDXRef-B.
    -- A file package.json is the dependency manifest of a package foo. Note that only one manifest should be used to define the same dependency graph.
  | BUILD_DEPENDENCY_OF
    -- Is to be used when SPDXRef-A is a build dependency of SPDXRef-B.
    -- A is in the compile scope of B in a Maven project.
  | DEV_DEPENDENCY_OF
    -- Is to be used when SPDXRef-A is a development dependency of SPDXRef-B.
    -- A is in the devDependencies scope of B in a Maven project.
  | OPTIONAL_DEPENDENCY_OF
    -- Is to be used when SPDXRef-A is an optional dependency of SPDXRef-B.
    -- Use when building the code will proceed even if a dependency cannot be found, fails to install, or is only installed on a specific platform. For example, A is in the optionalDependencies scope of npm project B.
  | PROVIDED_DEPENDENCY_OF
    -- Is to be used when SPDXRef-A is a to be provided dependency of SPDXRef-B.
    -- A is in the provided scope of B in a Maven project, indicating that the project expects it to be provided, for instance, by the container or JDK.
  | TEST_DEPENDENCY_OF
    -- Is to be used when SPDXRef-A is a test dependency of SPDXRef-B.
    -- A is in the test scope of B in a Maven project.
  | RUNTIME_DEPENDENCY_OF
    -- Is to be used when SPDXRef-A is a dependency required for the execution of SPDXRef-B.
    -- A is in the runtime scope of B in a Maven project.
  | EXAMPLE_OF
    -- Is to be used when SPDXRef-A is an example of SPDXRef-B.
    -- The file or snippet that illustrates how to use an application or library.
  | GENERATES
    -- Is to be used when SPDXRef-A generates SPDXRef-B.
    -- A SOURCE file makefile.mk generates a BINARY file a.out
  | GENERATED_FROM
    -- Is to be used when SPDXRef-A was generated from SPDXRef-B.
    -- A BINARY file a.out has been generated from a SOURCE file makefile.mk. A BINARY file foolib.a is generated from a SOURCE file bar.c.
  | ANCESTOR_OF
    -- Is to be used when SPDXRef-A is an ancestor (same lineage but pre-dates) SPDXRef-B.
    -- A SOURCE file makefile.mk is a version of the original ancestor SOURCE file ‘makefile2.mk’
  | DESCENDANT_OF
    -- Is to be used when SPDXRef-A is a descendant of (same lineage but postdates) SPDXRef-B.
    -- A SOURCE file makefile2.mk is a descendant of the original SOURCE file ‘makefile.mk’
  | VARIANT_OF
    -- Is to be used when SPDXRef-A is a variant of (same lineage but not clear which came first) SPDXRef-B.
    -- A SOURCE file makefile2.mk is a variant of SOURCE file makefile.mk if they differ by some edit, but there is no way to tell which came first (no reliable date information).
  | DISTRIBUTION_ARTIFACT
    -- Is to be used when distributing SPDXRef-A requires that SPDXRef-B also be distributed.
    -- A BINARY file foo.o requires that the ARCHIVE file bar-sources.tgz be made available on distribution.
  | PATCH_FOR
    -- Is to be used when SPDXRef-A is a patch file for (to be applied to) SPDXRef-B.
    -- A SOURCE file foo.diff is a patch file for SOURCE file foo.c.
  | PATCH_APPLIED
    -- Is to be used when SPDXRef-A is a patch file that has been applied to SPDXRef-B.
    -- A SOURCE file foo.diff is a patch file that has been applied to SOURCE file ‘foo-patched.c’.
  | COPY_OF
    -- Is to be used when SPDXRef-A is an exact copy of SPDXRef-B.
    -- A BINARY file alib.a is an exact copy of BINARY file a2lib.a.
  | FILE_ADDED
    -- Is to be used when SPDXRef-A is a file that was added to SPDXRef-B.
    -- A SOURCE file foo.c has been added to package ARCHIVE bar.tgz.
  | FILE_DELETED
    -- Is to be used when SPDXRef-A is a file that was deleted from SPDXRef-B.
    -- A SOURCE file foo.diff has been deleted from package ARCHIVE bar.tgz.
  | FILE_MODIFIED
    -- Is to be used when SPDXRef-A is a file that was modified from SPDXRef-B.
    -- A SOURCE file foo.c has been modified from SOURCE file foo.orig.c.
  | EXPANDED_FROM_ARCHIVE
    -- Is to be used when SPDXRef-A is expanded from the archive SPDXRef-B.
    -- A SOURCE file foo.c, has been expanded from the archive ARCHIVE file xyz.tgz.
  | DYNAMIC_LINK
    -- Is to be used when SPDXRef-A dynamically links to SPDXRef-B.
    -- An APPLICATION file ‘myapp’ dynamically links to BINARY file zlib.so.
  | STATIC_LINK
    -- Is to be used when SPDXRef-A statically links to SPDXRef-B.
    -- An APPLICATION file ‘myapp’ statically links to BINARY zlib.a.
  | DATA_FILE_OF
    -- Is to be used when SPDXRef-A is a data file used in SPDXRef-B.
    -- An IMAGE file ‘kitty.jpg’ is a data file of an APPLICATION ‘hellokitty’.
  | TEST_CASE_OF
    -- Is to be used when SPDXRef-A is a test case used in testing SPDXRef-B.
    -- A SOURCE file testMyCode.java is a unit test file used to test an APPLICATION MyPackage.
  | BUILD_TOOL_OF
    -- Is to be used when SPDXRef-A is used to build SPDXRef-B.
    -- A SOURCE file makefile.mk is used to build an APPLICATION ‘zlib’.
  | DEV_TOOL_OF
    -- Is to be used when SPDXRef-A is used as a development tool for SPDXRef-B.
    -- Any tool used for development such as a code debugger.
  | TEST_OF
    -- Is to be used when SPDXRef-A is used for testing SPDXRef-B.
    -- Generic relationship for cases where it's clear that something is used for testing but unclear whether it's TEST_CASE_OF or TEST_TOOL_OF.
  | TEST_TOOL_OF
    -- Is to be used when SPDXRef-A is used as a test tool for SPDXRef-B.
    -- Any tool used to test the code such as ESlint.
  | DOCUMENTATION_OF
    -- Is to be used when SPDXRef-A provides documentation of SPDXRef-B.
    -- A DOCUMENTATION file readme.txt documents the APPLICATION ‘zlib’.
  | OPTIONAL_COMPONENT_OF
    -- Is to be used when SPDXRef-A is an optional component of SPDXRef-B.
    -- A SOURCE file fool.c (which is in the contributors directory) may or may not be included in the build of APPLICATION ‘atthebar’.
  | METAFILE_OF
    -- Is to be used when SPDXRef-A is a metafile of SPDXRef-B.
    -- A SOURCE file pom.xml is a metafile of the APPLICATION ‘Apache Xerces’.
  | PACKAGE_OF
    -- Is to be used when SPDXRef-A is used as a package as part of SPDXRef-B.
    -- A Linux distribution contains an APPLICATION package gawk as part of the distribution MyLinuxDistro.
  | AMENDS
    -- Is to be used when (current) SPDXRef-DOCUMENT amends the SPDX information in SPDXRef-B.
    -- (Current) SPDX document A version 2 contains a correction to a previous version of the SPDX document A version 1. Note the reserved identifier SPDXRef-DOCUMENT for the current document is required.
  | PREREQUISITE_FOR
    -- Is to be used when SPDXRef-A is a prerequisite for SPDXRef-B.
    -- A library bar.dll is a prerequisite or dependency for APPLICATION foo.exe
  | HAS_PREREQUISITE
    -- Is to be used when SPDXRef-A has as a prerequisite SPDXRef-B.
    -- An APPLICATION foo.exe has prerequisite or dependency on bar.dll
  | OTHER
    -- Is to be used for a relationship which has not been defined in the formal SPDX specification. A description of the relationship should be included in the Relationship comments field.
  deriving (Eq, Show)

data Relation
  = Relation
  { _getRelationSrc :: Identifier
  , _getRelationType :: RelationType
  , _getRelationTarget :: Identifier
  } deriving (Eq)
instance Show Relation where
  show (Relation rSrc rType rTarget) = "{{{" ++ show rSrc ++ " >" ++ show rType ++ "> " ++ show rTarget ++ "}}}"

flipDirection :: Relation -> Relation
flipDirection (r@Relation{_getRelationSrc = src, _getRelationTarget = target}) = r{_getRelationSrc = target, _getRelationTarget = src}
{-|
  direction should always be from the smaller to the bigger in which it is included
-}
normalizeRelation :: Relation -> Relation
normalizeRelation (r@Relation{_getRelationType = DEPENDS_ON})       = flipDirection (r{_getRelationType = DEPENDENCY_OF})
normalizeRelation (r@Relation{_getRelationType = DESCRIBED_BY})     = flipDirection (r{_getRelationType = DESCRIBES})
normalizeRelation (r@Relation{_getRelationType = CONTAINS})         = flipDirection (r{_getRelationType = CONTAINED_BY})
normalizeRelation (r@Relation{_getRelationType = HAS_PREREQUISITE}) = flipDirection (r{_getRelationType = PREREQUISITE_FOR})
normalizeRelation r                                                 = r
