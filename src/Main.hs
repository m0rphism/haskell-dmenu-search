{-# LANGUAGE UnicodeSyntax, LambdaCase, FlexibleContexts, TemplateHaskell #-}

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import System.Environment
import System.Exit
import System.Process
import qualified DMenu

data Opts = Opts
  { _optsBrowser :: String
  , _optsEngines :: [(String, String)] -- ^ [(EngineName, EngineURLPrefix)]
  }

makeLenses ''Opts

pairs :: [a] → Either String [(a, a)]
pairs = \case
  []     → Right []
  x:y:xs → ((x,y):) <$> pairs xs
  _      → Left "Found search engine name without URL prefix."

parseEngines :: String -> Either String [(String, String)]
parseEngines es = pairs relevantLines
 where
  isEmptyLine = all (`elem` [' ', '\t'])
  relevantLines = filter (not . isEmptyLine) (lines es)

-- | Parse the command line arguments
readArgs
  :: [String] -- ^ Arguments from 'getArgs'
  -> IO Opts
readArgs args =
  execStateT (go $ words $ unwords args) (Opts "chromium -new-window" [])
 where
  go []
    = pure ()
  go (a:as)
    | a == "--"
    = pure () -- All arguments after "--" are passed to dmenu later.
    | a `elem` ["-b", "--browser"]
    , browser : as' ← as
    = do optsBrowser .= browser; go as'
    | a `elem` ["-e", "--engine"]
    , (name : urlPrefix : as') ← as
    = do optsEngines %= (++[(name, urlPrefix)]); go as'
    | a `elem` ["-E", "--engine-file"]
    , (filePath : as') ← as
    = liftIO (parseEngines <$> readFile filePath) >>= \case
        Left err → liftIO $ putStrLn $ "Failed to parse config file `"
                                    ++ filePath ++ "`: " ++ err
        Right es → do optsEngines %= (++es); go as'
    | a `elem` ["-h", "--help"]
    = liftIO $ do putStrLn usage; exitFailure
    | a == ""
    = go as
  go _
    = liftIO $ do putStrLn usage; exitFailure

main :: IO ()
main = do
  opts ← readArgs =<< getArgs
  let cfg1 = do DMenu.prompt .= "search with"; DMenu.forwardExtraArgs
  let cfg2 = do DMenu.prompt .= "search for"; DMenu.forwardExtraArgs
  DMenu.selectWith cfg1 fst (opts^.optsEngines) >>= \case
    Right (_, urlPrefix) →
      DMenu.select cfg2 [] >>= \case
        Right query →
          callCommand $ (opts^.optsBrowser) ++ " " ++ show (urlPrefix ++ query)
        _→ pure ()
    _ → pure ()

usage :: String
usage = unlines
  [ "USAGE"
  , "  dmenu-search [OPTIONS] [-- DMENUOPTIONS]"
  , ""
  , "  Let's the user choose a search engine and enter a search string by"
  , "  spawning two subsequent dmenu processes, and opens the resulting"
  , "  URL in a browser."
  , ""
  , "  All arguments, after the first `--` argument, are directly passed to dmenu."
  , ""
  , "OPTIONS"
  , "  -b, --browser CMD"
  , "    Shell command to open url in browser. Default: `chromium -new-window`"
  , ""
  , "  -e, --engine NAME URLPREFIX"
  , "    Add a search engine, e.g."
  , ""
  , "        -e google https://www.google.com/search?q="
  , "        -e github https://github.com/search?q="
  , ""
  , "  -E, --engine-file PATH"
  , "    Add search engines from a file."
  , ""
  , "    The following shows example content of an engine file:"
  , ""
  , "        google"
  , "        https://www.google.com/search?q="
  , ""
  , "        github"
  , "        https://github.com/search?q="
  , ""
  , "  -h, --help"
  , "    Display this message."
  ]
