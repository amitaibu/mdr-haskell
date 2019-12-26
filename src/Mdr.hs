{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Mdr (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_mdr_haskell

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_mdr_haskell.version)
    "Create fake people"
    "Create dummy Mothers and Children, to populate MDR-GIT with more data"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> argument str ( metavar "filepath"
                        <> help "The full path to the root of the MDR-GIT reposiroty. Under it, we expect to find a `data` directory."
                        )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
