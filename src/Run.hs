{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Util (encodeMotherToFile, encodeChildToFile, fakeMother, fakeChild)
import Faker (defaultFakerSettings, setNonDeterministic, generateWithSettings)
import Data.UUID.V4 (nextRandom)
import Faker.Name (femaleFirstName)
import Faker.Combinators (listOf)

run :: RIO App ()
run = do
    let settings = setNonDeterministic defaultFakerSettings

    replicateM_ 100 $ do
        uuid <- liftIO $ nextRandom
        lastName <- liftIO $ generateWithSettings settings femaleFirstName
        children <- liftIO $ generateWithSettings settings $ listOf 3 $ fakeChild uuid lastName
        mother <- liftIO $ generateWithSettings settings $ fakeMother uuid lastName children
        encodeMotherToFile mother
        mapM_ (\child -> encodeChildToFile uuid child) children
