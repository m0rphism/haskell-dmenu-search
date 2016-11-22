# haskell-dmenu-search
dmenu script for searching the web with customizable search engines.

![dmenu-search screenshot 1](doc/dmenu-search-1.png)

![dmenu-search screenshot 2](doc/dmenu-search-2.png)

Requirements:
[dmenu](http://tools.suckless.org/dmenu/) or
[dmenu2](https://bitbucket.org/melek/dmenu2).

Installation:
  Either get the sources from this repository, or
  [from hackage](https://hackage.haskell.org/package/dmenu-pkill).

Usage:

*   `dmenu-search [OPTIONS]`

    Let's the user choose a search engine and enter a search string by
    spawning two subsequent `dmenu` processes, and open the resulting
    URL in a browser.

Options:

*   `-b, --browser CMD`

    Shell command to open url in browser. Default: `chromium -new-window`
*   `-e, --engines URLs`

    Specify a list of search engines, e.g.
    
        -e google https://www.google.com/search?q=
        -e github https://github.com/search?q=
*   `-E, --engine-file PATH`

    Specify a file path to load the engines from.

The application is build with the
[dmenu Haskell bindings](https://hackage.haskell.org/package/dmenu), which
support customizing the dmenu commandline options in a configuration file.
