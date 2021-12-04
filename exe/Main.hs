{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative ((<**>))
import Cabal.Optimization (Optimization(..))
import Cabal.Project (Project(..), readProject)
import Cabal.SourceRepo
  (SourceRepositoryPackage(..), sourceRepositoryPackageGrammar, srpHoist)
import qualified Data.List.Extra as List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (maybeToList)
import Data.Void (Void)
import Distribution.CabalSpecVersion (cabalSpecLatest)
import Distribution.FieldGrammar (prettyFieldGrammar)
import Distribution.Fields.Pretty (PrettyField(..), showFields)
import Distribution.Types.SourceRepo (KnownRepoType(..), RepoType(..))
import Network.URI (URI)
import Options.Applicative
  ( Parser, execParser, fullDesc, header, help, helper, info, metavar
  , progDesc, strArgument
  )
import System.Directory (makeAbsolute, withCurrentDirectory)
import System.FilePath (makeRelative, takeDirectory)
import System.Process (readProcess)
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (Doc)

newtype Options = Options
  { cabalProjectFile :: FilePath
    -- ^ The @cabal.project@ file containing submodules.
  } deriving stock Show

optionsParser :: Parser Options
optionsParser = Options
  <$> strArgument
      (  metavar "FILE"
      <> help "The cabal.project file containing submodules" )

main :: IO ()
main = execParser opts >>= mainWithOptions
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc spiel
     <> header spiel )

    spiel = "Convert a submodule-using cabal.project file to one that uses source-repository-packages"

mainWithOptions :: Options -> IO ()
mainWithOptions Options{cabalProjectFile} = do
  absProjectFile <- makeAbsolute cabalProjectFile
  let absProjectDir = takeDirectory absProjectFile
  withCurrentDirectory absProjectDir $ do
    pf <- readProject absProjectFile
    gitSubmodStatusLines <- lines <$> git ["submodule", "status"]
    submods <- traverse parseSubmodule gitSubmodStatusLines
    (pkgs, submods') <- classifyPackages (map fst $ prjPackages pf) submods
    let pf' = pf { prjPackages = pkgs
                 , prjSourceRepos = concatMap submodToSourceRepos submods'
                                 ++ prjSourceRepos pf
                 }
    print $ ppProject pf'

-- | Convert a 'Submodule' with subdirectories to an equivalent set of
-- 'SourceRepositoryPackages'.
submodToSourceRepos :: Submodule FilePath -> [SourceRepositoryPackage Maybe]
submodToSourceRepos (Submodule{submodURL, submodCommit, submodSubdirs}) =
  [ SourceRepositoryPackage
      { srpType = KnownRepoType Git
      , srpLocation = submodURL
      , srpTag = Just submodCommit
      , srpBranch = Nothing
      , srpSubdir = Just subdir
      }
  | subdir <- submodSubdirs
  ]

-- | A @git@ submodule.
data Submodule subdir = Submodule
  { submodFilePath :: FilePath
    -- ^ The location of the submodule checkout as an absolute path.
  , submodURL :: String
    -- ^ The URL of the submodule's upstream @git@ repo.
  , submodCommit :: String
    -- ^ The submodule commit hash.
  , submodSubdirs :: [subdir]
    -- ^ Subdirectories within the submodule.
  } deriving stock Show

-- | Parse a single line of output from @git submodule@. This uses
-- @git config --get remote.origin.url@ under the hood to determine the
-- submodule's URL.
parseSubmodule :: String -> IO (Submodule Void)
parseSubmodule line = do
  (commit, relPath) <-
    case words line of
      (commit:relPath:_) -> pure (commit, relPath)
      _ -> fail $ unlines
        [ "Unexpected output from `git submodule`:"
        , line
        ]
  absPath <- makeAbsolute relPath
  withCurrentDirectory absPath $ do
    url <- List.trim <$> git ["config", "--get", "remote.origin.url"]
    pure $ Submodule{ submodFilePath = absPath
                    , submodURL      = url
                    , submodCommit   = commit
                    , submodSubdirs  = []
                    }

-- @'classifyPackages' packagePaths submods@ determines which of the
-- @packagePaths@ in the @cabal.project@ file are subdirectories of the
-- @submods@ and returns a new set of 'Submodule's where the 'submodSubdirs'
-- have been set appropriately. This function also separately returns the
-- subset of @packagePaths@ that do not correspond to any submodule.
classifyPackages :: [FilePath] -> [Submodule Void]
                 -> IO ([FilePath], [Submodule FilePath])
classifyPackages packagePaths submods = do
  let submodMap :: Map String (Submodule FilePath)
      submodMap = Map.fromList $
                  map (\submod -> (submodURL submod, submod{submodSubdirs = []})) submods
  (packagePaths', submodMap') <- go packagePaths submodMap
  pure (packagePaths', Map.elems submodMap')
  where
    go :: [FilePath] -> Map String (Submodule FilePath)
       -> IO ([FilePath], Map String (Submodule FilePath))
    go [] submodMap = pure ([], submodMap)
    go (pkgPath:pkgPaths) submodMap = do
      (pkgPaths', submodMap') <- go pkgPaths submodMap
      absPkgPath <- makeAbsolute pkgPath
      pure $ case List.firstJust
                    (\submod ->
                      if submodFilePath submod `List.isInfixOf` absPkgPath
                         then Just (submod, makeRelative (submodFilePath submod) absPkgPath)
                         else Nothing)
                    submods of
        Nothing -> (pkgPath:pkgPaths', submodMap')
        Just (submod, cabalFilePath) ->
          let submodMap'' = Map.adjust (\s -> s{submodSubdirs = takeDirectory cabalFilePath : submodSubdirs s})
                                       (submodURL submod) submodMap'
          in (pkgPaths', submodMap'')

-- | Pretty-print a 'Project' as the contents of a @cabal.project@ file.
ppProject :: Project URI opt FilePath -> Doc
ppProject (Project{ prjPackages, prjConstraints
                  , prjAllowNewer, prjReorderGoals, prjMaxBackjumps
                  , prjOptimization, prjSourceRepos, prjOtherFields }) =
  PP.text $ showFields (const []) $
       map (\xs -> PrettyField () "packages" $ PP.text xs) prjPackages
    ++ map (\xs -> PrettyField () "constraints" $ PP.text xs) prjConstraints
    ++ map (\xs -> PrettyField () "allow-newer" $ PP.text xs) prjAllowNewer
    ++ [ PrettyField () "reorder-goals" $ PP.text "True"
       | prjReorderGoals
       ]
    ++ maybeToList (fmap (\bj -> PrettyField () "max-backjumps" $ PP.text $ show bj) prjMaxBackjumps)
    ++ maybeToList (case prjOptimization of
         OptimizationOn      -> Nothing
         OptimizationOff     -> Just $ PrettyField () "optimization" $ PP.text "False"
         OptimizationLevel l -> Just $ PrettyField () "optimization" $ PP.text $ show l)
    ++ map (\repo -> PrettySection () "source-repository-package" [] $
              prettyFieldGrammar cabalSpecLatest sourceRepositoryPackageGrammar $
              srpHoist maybeToList repo) prjSourceRepos
    ++ prjOtherFields

-- | Spawn a 'git' subprocess with the supplied arguments.
git :: [String] -> IO String
git args = readProcess "git" args []
