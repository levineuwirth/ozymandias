module Main where

import Hakyll (hakyll)
import qualified Config
import Site (rules)

main :: IO ()
main = do
    -- Force config evaluation early so a missing or malformed site.yaml
    -- fails loudly before Hakyll starts.
    Config.siteConfig `seq` return ()
    hakyll rules
