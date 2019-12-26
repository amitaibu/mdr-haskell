{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Util
  ( encodeMotherToFile
  , encodeChildToFile
  , fakeChild
  , fakeMother
  ) where

import Data.Yaml
import Import
import RIO.Directory (createDirectoryIfMissing)
import RIO.FilePath (takeDirectory)
import RIO.ByteString (writeFile)
import Faker (Fake)
import Faker.Name (femaleFirstName)
import Data.Text (unpack, toLower, pack)
import Data.UUID (UUID, toText)

fakeMother :: UUID -> Text -> [ Child ] -> Fake Mother
fakeMother uuid lastName children = do
    motherFirstName <- femaleFirstName
    let motherLastName = lastName
    let motherChildren = children
    let motherId = toText uuid
    pure $ Mother{..}

fakeChild :: UUID -> Text -> Fake Child
fakeChild uuid lastName = do
    childFirstName <- femaleFirstName
    let childLastName = lastName
    let childId = toText uuid <> "-" <> toLower childFirstName
    pure $ Child {..}

encodeMotherToFile :: Mother -> RIO App ()
encodeMotherToFile mother = do
    r <- ask
    let options = appOptions r
    let rootFilePath = pack $ optionsFilePath options

    let fp = rootFilePath <> "/data/mothers/" <> (motherId mother) <> "/id.yaml"
    logInfo $ ("encodeMotherToFile into file: " <> (displayShow fp))
    liftIO $ do
        createEmptyFile (unpack fp)
        encodeFile (unpack fp) mother

encodeChildToFile :: UUID -> Child -> RIO App ()
encodeChildToFile motherId_ child = do
    r <- ask
    let options = appOptions r
    let rootFilePath = pack $ optionsFilePath options

    let fp = rootFilePath <> "/data/children/" <> childId child <> "/id.yaml"
    logInfo $ ("-- encodeChildToFile into file: " <> (displayShow fp))
    liftIO $ do
        createEmptyFile (unpack fp)
        encodeFile (unpack fp) (ChildWithMotherId child motherId_)

createEmptyFile :: FilePath -> IO ()
createEmptyFile path = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path ""