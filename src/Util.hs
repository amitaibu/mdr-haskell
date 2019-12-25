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
import Data.Text (unpack, toLower)
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

encodeMotherToFile :: HasLogFunc env => Mother -> RIO env ()
encodeMotherToFile mother = do
    let fp = "/home/amitaibu/Sites/mdr-git/data/mothers/" <> (motherId mother) <> "/id.yaml"
    logInfo $ ("encodeMotherToFile into file: " <> (displayShow fp))
    liftIO $ do
        createEmptyFile (unpack fp)
        encodeFile (unpack fp) mother

encodeChildToFile :: HasLogFunc env => UUID -> Child -> RIO env ()
encodeChildToFile motherId_ child = do
    let fp = "/home/amitaibu/Sites/mdr-git/data/children/" <> childId child <> "/id.yaml"
    logInfo $ ("-- encodeChildToFile into file: " <> (displayShow fp))
    liftIO $ do
        createEmptyFile (unpack fp)
        encodeFile (unpack fp) (ChildWithMotherId child motherId_)

createEmptyFile :: FilePath -> IO ()
createEmptyFile path = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path ""