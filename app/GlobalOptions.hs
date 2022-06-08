module GlobalOptions
  ( module GlobalOptions,
  )
where

import Commands.Extra
import MiniJuvix.Prelude
import Options.Applicative

data GlobalOptions = GlobalOptions
  { _globalNoColors :: Bool,
    _globalShowNameIds :: Bool,
    _globalOnlyErrors :: Bool,
    _globalNoTermination :: Bool,
    _globalInputFiles :: [FilePath]
  }
  deriving stock (Eq, Show)

makeLenses ''GlobalOptions

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions =
  GlobalOptions
    { _globalNoColors = False,
      _globalShowNameIds = False,
      _globalOnlyErrors = False,
      _globalNoTermination = False,
      _globalInputFiles = []
    }

instance Semigroup GlobalOptions where
  o1 <> o2 =
    GlobalOptions
      { _globalNoColors = o1 ^. globalNoColors || o2 ^. globalNoColors,
        _globalShowNameIds = o1 ^. globalShowNameIds || o2 ^. globalShowNameIds,
        _globalOnlyErrors = o1 ^. globalOnlyErrors || o2 ^. globalOnlyErrors,
        _globalNoTermination = o1 ^. globalNoTermination || o2 ^. globalNoTermination,
        _globalInputFiles = o1 ^. globalInputFiles ++ o2 ^. globalInputFiles
      }

instance Monoid GlobalOptions where
  mempty = defaultGlobalOptions
  mappend = (<>)

parseGlobalFlags :: Parser GlobalOptions
parseGlobalFlags = do
  _globalNoColors <-
    switch
      ( long "no-colors"
          <> help "Disable ANSI formatting"
      )
  _globalShowNameIds <-
    switch
      ( long "show-name-ids"
          <> help "Show the unique number of each identifier when pretty printing"
      )
  _globalOnlyErrors <-
    switch
      ( long "only-errors"
          <> help "Only print errors in a uniform format (used by minijuvix-mode)"
      )
  _globalNoTermination <-
    switch
      ( long "no-termination"
          <> help "Disable termination checking"
      )

  return GlobalOptions {_globalInputFiles = [], ..}

parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions = do
  opts <- parseGlobalFlags
  files <- parserInputFiles
  return opts {_globalInputFiles = files}
