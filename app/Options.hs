module Options
  ( GAOOpts (..),
    parseOptions,
  )
where

import Control.Monad.IO.Class
import Control.Monad.State
import OptFunc
import Options.Applicative
import Types
import Utils

gaoopts :: Parser GAOOpts
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
      <*> option
        oMode
        ( long "optimization-mode"
            <> short 'o'
            <> help "What should we optimize for: vswr, gain, vswr+gain"
            <> showDefaultWith omodeShow
            <> value VSWRGAIN 
            <> metavar "omode"
        )
      <*> option
        dMode
        ( long "directional-mode"
            <> short 'y'
            <> help "Are we optimizing a symmetrical or directive antenna: symmetrical, directive"
            <> showDefaultWith dmodeShow
            <> value SYMMETRICAL
            <> metavar "dmode"
        )



oMode :: ReadM OptimizingMode
oMode = str >>= \s -> case s of
    "vswr"          -> return VSWR
    "gain"          -> return GAIN
    "vswr+gain"     -> return VSWRGAIN
    _ -> readerError "Accepted optimiziation modes are 'vswr', 'gain', and 'vswr+gain'."

dMode :: ReadM DirectiveMode
dMode = str >>= \s -> case s of
    "symmetrical"         -> return SYMMETRICAL
    "directive"          -> return DIRECTIVE
    _ -> readerError "Accepted directional modes are 'symmetrical' or 'directive'."


parseOptions :: GAO ()
parseOptions = do
  gopts <- liftIO $ execParser lopts
  modify (\s -> s {opts = gopts, genCount = initGenCount gopts, optfun = OF (optFunc (omode gopts) (dmode gopts))})
  where
    lopts =
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
