{-# LANGUAGE UnicodeSyntax, LambdaCase #-}

import Control.Lens
import System.Process

import qualified DMenu

templates :: [ (String, String) ]
templates =
  [ ("https://www.google.com/search?q=", "google")
  , ("https://www.haskell.org/hoogle/?hoogle=", "hoogle")
  , ("https://en.wikipedia.org/wiki/Special:Search?search=", "wikipedia (english)")
  , ("https://de.wikipedia.org/w/index.php?search=", "wikipedia (german)")
  , ("http://en.cppreference.com/mwiki/index.php?title=Special:Search&search=", "cppreference")
  , ("https://duckduckgo.com/?q=", "duckduckgo")
  , ("http://fddb.info/db/de/suche/?udd=0&cat=site-de&search=", "fddb")
  , ("https://github.com/search?q=", "github")
  , ("https://scholar.google.de/scholar?hl=en&q=", "scholar")
  , ("http://translate.google.com/?source=osdd#auto|auto|%s", "translate")
  , ("https://hackage.haskell.org/packages/search?terms=", "hackage")
  , ("http://hayoo.fh-wedel.de/?query=", "hayoo")
  , ("https://ixquick.com/do/dsearch?query=", "ixquick")
  , ("http://dict.leo.org/ende?lang=de&search=", "leo")
  , ("https://nixos.org/w/index.php?search=", "nixos wiki")
  , ("http://stackoverflow.com/search?q=", "stackoverflow")
  , ("http://stackexchange.com/search?q=", "stackexchange")
  , ("https://www.startpage.com/do/dsearch?query=", "startpage")
  , ("http://thepiratebay.org/search/", "thepiratebay")
  , ("https://twitter.com/search?q=", "twitter")
  , ("http://watchseries.ag/search/", "watchseries")
  , ("http://www.wolframalpha.com/input/?i=", "wolframalpha")
  , ("http://www.youtube.com/results?search_query=", "youtube")
  ]

browserCmd :: String
browserCmd = "chromium -new-window"

main :: IO ()
main = do
  DMenu.selectWith (DMenu.prompt .= "search with") snd templates >>= \case
    Right (template, _) →
      DMenu.select (DMenu.prompt .= "search for") [] >>= \case
        Right query → callCommand $ browserCmd ++ " " ++ show (template ++ query)
        _→ pure ()
    _ → pure ()
