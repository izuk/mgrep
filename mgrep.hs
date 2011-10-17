{-# LANGUAGE DeriveDataTypeable #-}

import qualified Data.ByteString.Lazy as B
import Data.List (tails)
import Data.Text.AhoCorasick
import Data.Version (showVersion)
import System.Console.CmdArgs

import Paths_mgrep

data MGrep = MGrep { invert :: Bool
                   , afterContext :: Int
                   , beforeContext :: Int
                   , patternFile :: String
                   , searchFiles :: [String]
                   } deriving (Show, Data, Typeable)

mgrep = cmdArgsMode $ MGrep
  { invert = False &= name "v"
  , afterContext = 0 &= name "A"
  , beforeContext = 0 &= name "B"
  , patternFile = def &= argPos 0 &= typ "PATTERNS"
  , searchFiles = def &= args &= typFile
  } &= summary "mgrep 0.1 (c) 2011 Itai Zukerman"
    &= details [ "Scan one or more files (or stdin) for lines matching a collection of substrings."
               ]
  
nlines = B.split 10

contexts before after xs = zip xs $ ps ++ cs
  where
    size = before + 1 + after
    ps = [take n xs | n <- [after+1 .. size-1]]
    cs = map (take size) $ tails xs

main = do args <- cmdArgsRun mgrep
          template <- fmap (compile . filter (not . B.null) . nlines) . B.readFile $ patternFile args
          let ins = if null $ searchFiles args 
                    then [B.getContents] 
                    else map B.readFile $ searchFiles args
              flt = (if invert args then (not .) else id) (search template)
              ios = map (>>= dumpMatches . scanString flt (beforeContext args) (afterContext args)) ins
          sequence ios
  where
    scanString flt before after = filter (flt . fst) . contexts before after . nlines
    dumpMatches = mapM_ (mapM_ B.putStrLn . snd)
