{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Config where

import           Data.Monoid
import           Control.Exception              ( Exception
                                                , try
                                                , throw
                                                )
import           Filesystem.Path               as FP
import           Filesystem.Path.CurrentOS     as FPCOS
import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( join
                                                , when
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , toJSON
                                                , eitherDecode
                                                )
import           Data.Aeson.Types               ( parseEither )
import           Data.Bifunctor                 ( bimap
                                                , first
                                                , second
                                                )
import           Data.ByteString.Lazy          as DBL
import           Data.Functor.Identity
import           Data.Text.IO                  as DTI
import           Data.Text                     as DT
import           GHC.Generics
import           Text.Parsec.Error              ( ParseError )
import           Text.Read                      ( readMaybe )
import           Text.Toml                      ( parseTomlDoc )


type Host = String
type Port = Int
newtype ProcessEnvironment = ProcessEnvironment {getProcessEnv :: [(String, String)]} deriving Eq

data ConfigurationError = ConfigParseError String
                        | TOMLParserError ParseError
                        | InvalidConfigError String
                        | InvalidPath FP.FilePath String
                          deriving (Eq)

deriving instance Show ConfigurationError
deriving instance Exception ConfigurationError

data Config f = Config
  { host :: f Host
  , port :: f Port
  }

type ConfigComplete = Config Identity
deriving instance Generic ConfigComplete
deriving instance Eq ConfigComplete
deriving instance Show ConfigComplete
deriving instance FromJSON ConfigComplete


type ConfigPartial = Config Maybe
deriving instance Generic ConfigPartial
deriving instance Eq ConfigPartial
deriving instance Show ConfigPartial
deriving instance FromJSON ConfigPartial

defaultHost :: Host
defaultHost = "localhost"

defaultPort :: Port
defaultPort = 8080

instance Semigroup ConfigComplete where
  a <> b = b

instance Monoid ConfigComplete where
  mempty = Config (Identity defaultHost) (Identity defaultPort)

instance Semigroup ConfigPartial where
  a <> b = Config { host = resolveMaybes host, port = resolveMaybes port }
   where
    resolveMaybes :: (ConfigPartial -> Maybe a) -> Maybe a
    resolveMaybes getter = getter b <|> getter a

instance Monoid ConfigPartial where
  mempty = Config Nothing Nothing

----------
-- JSON --
----------

class (FromJSON cfg) => FromJSONFile cfg where
    fromJSONFile :: FP.FilePath -> IO (Either ConfigurationError cfg)

instance FromJSONFile ConfigPartial where
  fromJSONFile path = decodeAndTransformError <$> DBL.readFile convertedPath
   where
    convertedPath = FPCOS.encodeString path
    decodeAndTransformError
      :: ByteString -> Either ConfigurationError ConfigPartial
    decodeAndTransformError = first ConfigParseError . eitherDecode

----------
-- TOML --
----------

class (FromJSONFile cfg) => FromTOMLFile cfg where
    fromTOMLFile :: FP.FilePath -> IO (Either ConfigurationError cfg)

instance FromTOMLFile ConfigPartial where
  fromTOMLFile path =
    flattenEither . convertAndParse . parseTOML <$> DTI.readFile convertedPath
   where
    convertedPath   = FPCOS.encodeString path
    parseTOML       = first TOMLParserError . parseTomlDoc ""
    convertAndParse = second (parseEither parseJSON . toJSON)
    flattenEither v = case v of
      Right (Right cfg) -> Right cfg
      Right (Left  err) -> Left (ConfigParseError err)
      Left  err         -> Left err

---------
-- ENV --
---------

class FromENV cfg where
    fromENV :: ProcessEnvironment -> cfg

instance FromENV ConfigPartial where
  fromENV pEnv = Config { host = prop "PERSONA_HOST"
                        , port = readMaybe =<< prop "PERSONA_PORT"
                        }
   where
    env :: [(String, String)]
    env = getProcessEnv pEnv

    prop :: String -> Maybe String
    prop = flip lookup env

mergeInPartial :: ConfigComplete -> ConfigPartial -> ConfigComplete
mergeInPartial c p = Config { host = maybe (host c) Identity (host p)
                            , port = maybe (port c) Identity (port p)
                            }

-- | Ensure that an Either resolves to it's Right value, ensure that a
rightOrThrow :: (Exception a) => Either a b -> IO b
rightOrThrow e = case e of
  (Left  err) -> throw err
  (Right v  ) -> return v

buildConfigWithDefault :: ConfigComplete -> [ConfigPartial] -> ConfigComplete
buildConfigWithDefault orig partials = orig `mergeInPartial` combinedPartials
 where
  combinedPartials :: ConfigPartial
  combinedPartials = Prelude.foldl (<>) (mempty :: ConfigPartial) partials

-- | Build an App configuration from a given file, using system environment as well as
makeAppConfig
  :: Maybe Prelude.FilePath
  -> ProcessEnvironment
  -> IO (Either ConfigurationError ConfigComplete)
makeAppConfig maybeStrPath env = try generateConfig
 where
  maybePath :: Maybe FPCOS.FilePath
  maybePath = FPCOS.fromText . DT.pack <$> maybeStrPath

  extension :: Maybe (Maybe Text)
  extension       = FP.extension <$> maybePath

  isJSONExtension = (== Just "json")
  isTOMLExtension = (== Just "toml")

  isJSONFile      = maybe False isJSONExtension extension
  isTOMLFile      = maybe False isTOMLExtension extension

  pathExtensionIsInvalid :: Bool
  pathExtensionIsInvalid = not $ isJSONFile || isTOMLFile

  pathInvalidExtensionErr :: ConfigurationError
  pathInvalidExtensionErr = InvalidPath
    (fromMaybe "<no path>" maybePath)
    "Path is invalid (must be either a .json or .toml path)"

  envCfg :: ConfigPartial
  envCfg = fromENV env :: ConfigPartial

  fullySpecifiedPartialCfg :: ConfigComplete
  fullySpecifiedPartialCfg = mergeInPartial mempty mempty

  buildFromEnv :: IO ConfigComplete
  buildFromEnv = pure $ mergeInPartial fullySpecifiedPartialCfg envCfg

  getFileConfig
    :: FPCOS.FilePath -> IO (Either ConfigurationError ConfigPartial)
  getFileConfig = if isJSONFile then fromJSONFile else fromTOMLFile

  generateConfig :: IO ConfigComplete
  generateConfig = maybe buildFromEnv buildFromPathAndEnv maybePath

  buildFromPathAndEnv :: FPCOS.FilePath -> IO ConfigComplete
  buildFromPathAndEnv path =
    when pathExtensionIsInvalid (throw pathInvalidExtensionErr)
      >>  getFileConfig path
      >>= rightOrThrow
      >>= \fileCfg ->
            pure
              (buildConfigWithDefault (mempty :: ConfigComplete)
                                      [fileCfg, envCfg]
              )
