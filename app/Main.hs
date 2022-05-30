module Main (main) where

import App
import CLI
import Commands.Termination as Termination
import Control.Exception qualified as IO
import Control.Monad.Extra
import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Pipeline
import MiniJuvix.Prelude hiding (Doc)
import MiniJuvix.Prelude.Pretty hiding (Doc)
import MiniJuvix.Syntax.Abstract.InfoTable qualified as Abstract
import MiniJuvix.Syntax.Abstract.Language qualified as Abstract
import MiniJuvix.Syntax.Abstract.Pretty qualified as Abstract
import MiniJuvix.Syntax.Concrete.Parser qualified as Parser
import MiniJuvix.Syntax.Concrete.Scoped.Highlight qualified as Highlight
import MiniJuvix.Syntax.Concrete.Scoped.InfoTable qualified as Scoper
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as Scoper
import MiniJuvix.Syntax.Concrete.Scoped.Pretty qualified as Scoper
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Html
import MiniJuvix.Syntax.Concrete.Scoped.Scoper qualified as Scoper
import MiniJuvix.Syntax.MicroJuvix.Pretty qualified as Micro
import MiniJuvix.Syntax.MicroJuvix.TypeChecker qualified as MicroTyped
import MiniJuvix.Syntax.MiniHaskell.Pretty qualified as MiniHaskell
import MiniJuvix.Syntax.MonoJuvix.Pretty qualified as Mono
import MiniJuvix.Termination qualified as Termination
import MiniJuvix.Translation.AbstractToMicroJuvix qualified as Micro
import MiniJuvix.Translation.MicroJuvixToMonoJuvix qualified as Mono
import MiniJuvix.Translation.MonoJuvixToMiniC qualified as MiniC
import MiniJuvix.Translation.MonoJuvixToMiniHaskell qualified as MiniHaskell
import MiniJuvix.Translation.ScopedToAbstract qualified as Abstract
import MiniJuvix.Utils.Version (runDisplayVersion)
import Options.Applicative
import Text.Show.Pretty hiding (Html)

minijuvixYamlFile :: FilePath
minijuvixYamlFile = "minijuvix.yaml"

findRoot :: CLI -> IO FilePath
findRoot cli = do
  setCurrentDirectory (takeDirectory (cliMainFile cli))
  r <- IO.try go :: IO (Either IO.SomeException FilePath)
  case r of
    Left err -> do
      putStrLn "Something went wrong when figuring out the root of the project."
      putStrLn (pack (IO.displayException err))
      cur <- getCurrentDirectory
      putStrLn ("I will try to use the current directory: " <> pack cur)
      return cur
    Right root -> return root
  where
    possiblePaths :: FilePath -> [FilePath]
    possiblePaths start = takeWhile (/= "/") (aux start)
      where
        aux f = f : aux (takeDirectory f)

    go :: IO FilePath
    go = do
      c <- getCurrentDirectory
      l <- findFile (possiblePaths c) minijuvixYamlFile
      case l of
        Nothing -> return c
        Just yaml -> return (takeDirectory yaml)

getEntryPoint :: FilePath -> GlobalOptions -> EntryPoint
getEntryPoint r opts =
  EntryPoint
    { _entryPointRoot = r,
      _entryPointNoTermination = opts ^. globalNoTermination,
      _entryPointModulePaths = opts ^. globalInputFiles
    }

runCLI :: Members '[Embed IO, App] r => CLI -> Sem r ()
runCLI cli = do
  let globalOptions = cli ^. cliGlobalOptions
      inputFile = cliMainFile cli
      toAnsiText' :: forall a. (HasAnsiBackend a, HasTextBackend a) => a -> Text
      toAnsiText' = toAnsiText (not (globalOptions ^. globalNoColors))
  root <- embed (findRoot cli)
  let entryPoint = getEntryPoint root globalOptions
  case cli ^. cliCommand of
    DisplayVersion -> embed runDisplayVersion
    DisplayRoot -> say (pack root)
    Highlight -> do
      res <- runPipelineEither (upToScoping (getEntryPoint root globalOptions))
      absP <- embed (makeAbsolute inputFile)
      case res of
        Left err -> say (Highlight.goError (errorIntervals err))
        Right r -> do
          let tbl = r ^. Scoper.resultParserTable
              items = tbl ^. Parser.infoParsedItems
              names = r ^. (Scoper.resultScoperTable . Scoper.infoNames)
              hinput =
                Highlight.filterInput
                  absP
                  Highlight.HighlightInput
                    { _highlightNames = names,
                      _highlightParsed = items
                    }
          say (Highlight.go hinput)
    Parse opts -> do
      m <-
        head . (^. Parser.resultModules)
          <$> runPipeline (upToParsing (getEntryPoint root globalOptions))
      if opts ^. parseNoPrettyShow then say (show m) else say (pack (ppShow m))
    Scope opts -> do
      l <- (^. Scoper.resultModules) <$> runPipeline (upToScoping (getEntryPoint root globalOptions))
      forM_ l $ \s -> do
        renderStdOut (Scoper.ppOut (mkScopePrettyOptions globalOptions opts) s)
    Html HtmlOptions {..} -> do
      res <- runPipeline (upToScoping (getEntryPoint root globalOptions))
      let m = head (res ^. Scoper.resultModules)
      embed (genHtml Scoper.defaultOptions _htmlRecursive _htmlTheme m)
    MicroJuvix Pretty -> do
      micro <- head . (^. Micro.resultModules) <$> runPipeline (upToMicroJuvix entryPoint)
      let ppOpts =
            Micro.defaultOptions
              { Micro._optShowNameId = globalOptions ^. globalShowNameIds
              }
      App.renderStdOut (Micro.ppOut ppOpts micro)
    MicroJuvix (TypeCheck opts) -> do
      res <- runPipeline (upToMicroJuvixTyped entryPoint)
      say "Well done! It type checks"
      when (opts ^. microJuvixTypePrint) $ do
        let ppOpts =
              Micro.defaultOptions
                { Micro._optShowNameId = globalOptions ^. globalShowNameIds
                }
            checkedModule = head (res ^. MicroTyped.resultModules)
        renderStdOut (Micro.ppOut ppOpts checkedModule)
        newline
        let typeCalls = Mono.buildTypeCallMap res
        renderStdOut (Micro.ppOut ppOpts typeCalls)
        newline
        let concreteTypeCalls = Mono.collectTypeCalls res
        renderStdOut (Micro.ppOut ppOpts concreteTypeCalls)
    MonoJuvix -> do
      let ppOpts =
            Mono.defaultOptions
              { Mono._optShowNameIds = globalOptions ^. globalShowNameIds
              }
      monojuvix <- head . (^. Mono.resultModules) <$> runPipeline (upToMonoJuvix entryPoint)
      renderStdOut (Mono.ppOut ppOpts monojuvix)
    MiniHaskell -> do
      minihaskell <- head . (^. MiniHaskell.resultModules) <$> runPipeline (upToMiniHaskell entryPoint)
      renderStdOut (MiniHaskell.ppOutDefault minihaskell)
    MiniC -> do
      miniC <- (^. MiniC.resultCCode) <$> runPipeline (upToMiniC entryPoint)
      say miniC
    Compile o -> do
      miniC <- (^. MiniC.resultCCode) <$> runPipeline (upToMiniC entryPoint)
      result <- embed (runCompile root inputFile o miniC)
      case result of
        Left err -> say ("Error: " <> err)
        _ -> return ()
    Termination (Calls opts@CallsOptions {..}) -> do
      results <- runPipeline (upToAbstract entryPoint)
      let topModule = head (results ^. Abstract.resultModules)
          infotable = results ^. Abstract.resultTable
          callMap0 = Termination.buildCallMap infotable topModule
          callMap = case _callsFunctionNameFilter of
            Nothing -> callMap0
            Just f -> Termination.filterCallMap f callMap0
          opts' = Termination.callsPrettyOptions opts
      renderStdOut (Abstract.ppOut opts' callMap)
      newline
    Termination (CallGraph CallGraphOptions {..}) -> do
      results <- runPipeline (upToAbstract entryPoint)
      let topModule = head (results ^. Abstract.resultModules)
          infotable = results ^. Abstract.resultTable
          callMap = Termination.buildCallMap infotable topModule
          opts' =
            Abstract.defaultOptions
              { Abstract._optShowNameId = globalOptions ^. globalShowNameIds
              }
          completeGraph = Termination.completeCallGraph callMap
          filteredGraph = maybe completeGraph (`Termination.unsafeFilterGraph` completeGraph) _graphFunctionNameFilter
          rEdges = Termination.reflexiveEdges filteredGraph
          recBehav = map Termination.recursiveBehaviour rEdges
      App.renderStdOut (Abstract.ppOut opts' filteredGraph)
      newline
      forM_ recBehav $ \r -> do
        let funName = r ^. Termination.recursiveBehaviourFun
            funRef = Abstract.FunctionRef (Scoper.unqualifiedSymbol funName)
            funInfo = HashMap.lookupDefault impossible funRef (infotable ^. Abstract.infoFunctions)
            markedTerminating = funInfo ^. (Abstract.functionInfoDef . Abstract.funDefTerminating)
            sopts =
              Scoper.defaultOptions
                { Scoper._optShowNameId = globalOptions ^. globalShowNameIds
                }
            n = toAnsiText' (Scoper.ppOut sopts funName)
        App.renderStdOut (Abstract.ppOut opts' r)
        newline
        if
            | markedTerminating -> say (n <> " Terminates by assumption")
            | otherwise ->
                case Termination.findOrder r of
                  Nothing -> say (n <> " Fails the termination checking") >> embed exitFailure
                  Just (Termination.LexOrder k) -> say (n <> " Terminates with order " <> show (toList k))
        newline

main :: IO ()
main = do
  cli <- execParser descr >>= makeAbsPaths
  runM (runAppIO (cli ^. cliGlobalOptions) (runCLI cli))
