module Data.Text.AhoCorasick (
  compile,
  search
  ) where

--import Test.QuickCheck

import Data.Function (on)
import Data.List (groupBy, sort)
import Data.Maybe (fromMaybe)
import Data.Word
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as M

type Children a = M.IntMap a

data Trie = Root (Children Trie)
          | Node Bool (Children Trie) Trie
            
instance Show Trie where
  show (Root m) = "Root " ++ show (M.keysSet m)
  show (Node t m _) = "Node " ++ show t ++ " " ++ show (M.keysSet m)

goto :: Trie -> Word8 -> Maybe Trie
goto (Root m) ch = M.lookup (fromIntegral ch) m
goto (Node _ m _) ch = M.lookup (fromIntegral ch) m

compile :: [B.ByteString] -> Trie
compile patterns = if any B.null patterns
                   then error "no null patterns"
                   else root
    where
        root = Root (M.fromList . map f $ level1)
        level1 = partition . sort $ patterns
        f (ch, xs) = (fromIntegral ch, Node (any B.null xs) (expand root xs) root)

expand :: Trie -> [B.ByteString] -> Children Trie
expand failure = M.fromList . map f . partition
  where
    f p@(ch, _) = (fromIntegral ch, buildNode failure p)

buildNode :: Trie -> (Word8, [B.ByteString]) -> Trie
buildNode parentFailure (ch, xs) = Node (any B.null xs) (expand failure xs) failure
  where
    failure = backtrack parentFailure
    backtrack n =
      goto n ch `orElse` case n of
        Root _ -> n
        Node _ _ failure' -> backtrack failure'

search r s = search' r 0
  where
    size = B.length s
    search' (Node True _ _) _ = True
    search' n idx | idx == size = loop n
      where
        loop (Root _) = False
        loop (Node True _ _) = True
        loop (Node _ _ failure) = loop failure
    search' r@(Root _) idx = 
      case goto r (s `B.index` idx) of
        Just n -> search' n (idx + 1)
        Nothing -> search' r (idx + 1)
    search' n@(Node _ _ failure) idx =
      case goto n (s `B.index` idx) of
        Just n -> search' n (idx + 1)
        Nothing -> search' failure idx

-- Utils
        
orElse :: Maybe a -> a -> a        
orElse = flip fromMaybe

partition :: [B.ByteString] -> [(Word8, [B.ByteString])]
partition xs = splits
  where
    fulls = filter (not . B.null) xs
    firsts = groupBy ((==) `on` B.head) fulls
    splits = map (\ys -> (B.head $ head ys, map B.tail ys)) firsts

-- QuickCheck

{-
instance Arbitrary B.ByteString where
  arbitrary = sized $ \n ->
    fmap B.pack $ sequence [arbitrary | _ <- [1..n]]

prop_find pre post xs = all ((> 0) . B.length) xs ==>
                          let t = compile xs
                          in and [search t (B.concat [pre, x, post]) | x <- xs]
-}