module Data.Text.RabinKarp (
  dict,
  matches
  ) where

import Test.QuickCheck

import Data.Int
import Data.List (find)
import Data.Maybe (isJust)
import Data.Word

import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as M

bsLines = B.split 10

bsJoin :: [B.ByteString] -> B.ByteString
bsJoin = B.intercalate (B.singleton 10)

radix = 257 
modValue = 160481219

data RK = RK { size :: Int
             , tailMultiple :: Int
             , hashMap :: M.IntMap [B.ByteString]
             } deriving (Show)

roll :: Int -> Word8 -> Int
roll hash ch = (hash * radix + fromIntegral ch) `rem` modValue

hash :: B.ByteString -> Int
hash = B.foldl roll 0

hashes :: RK -> B.ByteString -> [Int]
hashes (RK size tailMultiple _) s | fromIntegral l < size = []
                                  | otherwise = f h0 n
  where
    n = fromIntegral size
    h0 = hash $ B.take n s
    l = B.length s
    f h i | i == l = [h]
          | otherwise = let ch1 = fromIntegral $ B.index s i
                            ch2 = fromIntegral $ B.index s (i-n)
                            h' = ((h - tailMultiple * ch2) * radix + ch1) `rem` modValue
                            h'' = if h' < 0 then h' + modValue else h'
                        in h : f h'' (i+1)

matches :: RK -> B.ByteString -> Bool
matches rk@(RK _ _ m) s = go 0 $ hashes rk s
  where
    go _ [] = False
    go i (h:hs) = case M.lookup h m of
      Nothing -> go (i+1) hs
      Just bss -> let s' = B.drop i s
                  in any (`B.isPrefixOf` s') bss || go (i+1) hs

calcTail size = foldl roll 1 $ replicate (size-1) 0

dict :: [B.ByteString] -> RK
dict bss = RK { size = fromIntegral sz
              , tailMultiple = calcTail $ fromIntegral sz
              , hashMap = m
              }
  where
    bss' = filter (not . B.null) bss
    sz = minimum (map B.length bss')
    m = foldl f M.empty $ map (\bs -> (bs, hash $ B.take sz bs)) bss'
    f m (s, h) = M.insertWith (++) h [s] m

instance Arbitrary B.ByteString where
  arbitrary = sized $ \n ->
    fmap B.pack $ sequence [arbitrary | _ <- [1..n]]

testSize = 4

emptyRK = RK { size = testSize
             , tailMultiple = calcTail testSize
             , hashMap = M.empty
             }
  
prop_hashNonNeg bs = hash bs >= 0

prop_hashesNonNeg bs = B.length bs >= 4 ==> all (>= 0) $ hashes emptyRK bs

prop_hashTail bs = B.length bs >= (fromIntegral $ size emptyRK)+1 ==> 
                     (tail $ hashes emptyRK bs) == hashes emptyRK $ B.tail bs

prop_dictConsistent bss = all ((>= 4) . B.length) bss ==> 
                            let rk = dict bss
                            in all id [all (h ==) (map (hash . B.take (fromIntegral $ size rk)) words)  | (h, words) <- M.assocs (hashMap rk)]

prop_hashMiddle b1 bs b2 = B.length bs >= (fromIntegral $ size emptyRK) ==>
                             let hs = hashes emptyRK $ B.concat [b1, bs, b2]
                                 h = hash $ B.take (fromIntegral $ size emptyRK) bs
                             in isJust $ find (== h) hs

prop_run m1 m2 m3 bss1 bss2 = all ((>= 4) . B.length) (concat [[m1, m2, m3], bss1, bss2]) ==>
                                let rk = dict [m1, m2, m3]
                                in matches rk $ bsJoin $ concat [bss1, [m2], bss2]