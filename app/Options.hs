module Options
  ( GAOOpts (..),
    parseOptions,
  )
where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Semigroup ((<>))
import Options.Applicative
import Types

gaoopts =
  GAOOpts
    <$> strOption
      ( long "gaofile"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "GAO Model to optimize"
      )
    <*> ( length
            <$> many
              ( flag'
                  ()
                  ( short 'v'
                      <> help "How verbose to optimize (can be specified multiple times)"
                      <> showDefault
                  )
              )
        )
    <*> switch
      ( short 'd'
          <> long "select-distinct"
          <> help "Select only distinct individuals as survivors"
      )
    <*> option
      auto
      ( long "population-size"
          <> short 's'
          <> help "How many individuals are in one generation"
          <> showDefault
          <> value 20
          <> metavar "INT"
      )
    <*> option
      auto
      ( long "generation-count"
          <> short 'c'
          <> help "How many generations to run the optimizer"
          <> showDefault
          <> value 10
          <> metavar "INT"
      )

parseOptions :: GAO ()
parseOptions = do
  gopts <- liftIO $ execParser opts
  modify (\s -> s {opts = gopts, genCount = initGenCount gopts})
  where
    opts =
      info
        (gaoopts <**> helper)
        ( fullDesc
            <> progDesc "Run an optimizer for GAOModel FILENAME"
            <> header "xnec2c-gao - a genetic algorithm optimizer for your antenna model"
            <> footer
              "Copyright 2022 Maurizio Di Pietro DC1MDP. Program is provided \"as is\". \
              \Author is not responsible for any havoc caused by the usage of this software. \
              \Use at own risk."
        )
