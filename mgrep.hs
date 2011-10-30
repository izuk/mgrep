{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Control.Applicative
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.List (tails)
import Data.Text.AhoCorasick
import System.Console.CmdArgs

data MGrep = MGrep { invert :: Bool
                   , afterContext :: Int
                   , beforeContext :: Int
                   , patternFile :: String
                   , searchFiles :: [String]
                   } deriving (Show, Data, Typeable)

mgrep :: MGrep
mgrep = MGrep
  { invert = False &= name "v"
  , afterContext = 0 &= name "A"
  , beforeContext = 0 &= name "B"
  , patternFile = def &= argPos 0 &= typ "PATTERNS"
  , searchFiles = def &= args &= typFile
  } &= summary "MGrep 0.1 (c) 2011 Itai Zukerman"
    &= help "Scan one or more files (or stdin) for lines matching a collection of substrings from a patterns file."
    &= details [ "MGrep uses the Aho-Corasick algorithm for O(n) scanning.  In practice this can be quite fast, even for large substring sets."
               ]

lines10 :: ByteString -> [ByteString]
lines10 = B.split 10

contexts :: Int -> Int -> [a] -> [(a, [a])]
contexts before after xs = zip xs $ ps ++ cs
  where
    size = before + 1 + after
    ps = [take n xs | n <- [after+1 .. size-1]]
    cs = take size <$> tails xs

scan :: (ByteString -> Bool) -> Int -> Int -> IO ByteString -> IO ()
scan f before after ss = ss >>= loop . contexts before after . lines10
  where
    loop :: [(ByteString, [ByteString])] -> IO ()
    loop [] = return ()
    loop ((s, ctx):st) = if f s
                         then mapM_ B.putStrLn ctx >> loop st
                         else loop st

main :: IO ()
main = do MGrep{..} <- cmdArgs mgrep
          patterns <- compile . filter (not . B.null) . lines10 <$> B.readFile patternFile
          let ins = if null searchFiles then [B.getContents] else B.readFile <$> searchFiles
              rev = if invert then not else id
              flt = rev . search patterns
          mapM_ (scan flt beforeContext afterContext) ins
