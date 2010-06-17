{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}
import System.IO.Unsafe (unsafePerformIO)
import Criterion.Main (defaultMain, bgroup, bench)
import System.IO
import System.Random
import qualified Data.List

main = defaultMain  [ bgroup "ascending ints" [bench "ghc" $ run' ghcSort, bench "yhc" $ run' yhcSort],

                          bgroup "des" [bench "ghc" $ run'' ghcSort, bench "yhc" $ run'' yhcSort],
                          bgroup "random" [bench "ghc" $ run''' ghcSort, bench "yhc" $ run''' yhcSort],
                          bgroup "shakespeare" [bench "ghc" $ run'''' ghcSort, bench "yhc" $ run'''' yhcSort]
                        ]

-- run', run'', run''', run'''' ::([a] -> [a]) -> IO Bool
run' = runOnce sortedNs
run'' = runOnce reverseSortedNs
run''' = runOnce randomNums
run'''' = runOnce shakespeare

runOnce :: (Ord a) => (Int -> t1) -> (t1 -> [a]) -> IO ()
{-# NOINLINE runOnce #-}
runOnce generator s  = putStr (if (isSorted ( s (generator (10^(6::Int))))) then "T" else "False") >> return ()

-- verify
isSorted x = x == Data.List.sort x

-- corpus
sortedNs x = take x [(1::Int)..]
reverseSortedNs = reverse . sortedNs

randomNums :: Int -> [Int]
randomNums x = take x $ randoms $ unsafePerformIO $ newStdGen

shakespeare = const $ unsafePerformIO $ readFile "shaks12.txt"

-- YHC functions
yhcSort :: (Ord a) => [a] -> [a]
yhcSort =  yhcSortBy compare
  where
    yhcSortBy :: (a -> a -> Ordering) -> [a] -> [a]
    yhcSortBy cmp = mergeAll . sequences
     where
        sequences (a:b:xs)
          | a `cmp` b == GT = descending b [a]  xs
          | otherwise       = ascending  b (a:) xs
        sequences xs = [xs]

        descending a as (b:bs)
          | a `cmp` b == GT = descending b (a:as) bs
        descending a as bs  = (a:as): sequences bs

        ascending a as (b:bs)
          | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
        ascending a as bs   = as [a]: sequences bs

        mergeAll [x] = x
        mergeAll xs  = mergeAll (mergePairs xs)

        mergePairs (a:b:xs) = merge a b: mergePairs xs
        mergePairs xs       = xs

        merge as@(a:as') bs@(b:bs')
          | a `cmp` b == GT = b:merge as  bs'
          | otherwise       = a:merge as' bs
        merge [] bs         = bs
        merge as []         = as

-- GHC functions
-- this is outdated as of GHC 6.12.2; see <http://hackage.haskell.org/trac/ghc/ticket/2143>
ghcSort :: forall a. (Ord a) => [a] -> [a]
ghcSort l = mergesort compare l
 where
    mergesort :: (a -> a -> Ordering) -> [a] -> [a]
    mergesort cmp = mergesort' cmp . map wrap

    mergesort' :: (a -> a -> Ordering) -> [[a]] -> [a]
    mergesort' _   [] = []
    mergesort' _   [xs] = xs
    mergesort' cmp xss = mergesort' cmp (merge_pairs cmp xss)

    merge_pairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
    merge_pairs _   [] = []
    merge_pairs _   [xs] = [xs]
    merge_pairs cmp (xs:ys:xss) = merge cmp xs ys : merge_pairs cmp xss

    merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
    merge _   [] ys = ys
    merge _   xs [] = xs
    merge cmp (x:xs) (y:ys)
     = case x `cmp` y of
            GT -> y : merge cmp (x:xs)   ys
            _  -> x : merge cmp    xs (y:ys)

    wrap :: a -> [a]
    wrap x = [x]
