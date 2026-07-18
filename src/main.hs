module Main (main) where
import Data.Numbers.Primes (primes)
import Data.Heap (Heap, Entry)
import qualified Data.Heap as Heap
import qualified Queue

main :: IO ()
main = print (findRun semiprimes 30 20)

findRun :: (Num a, Ord a) => [a] -> a -> a -> Maybe a
findRun s gap len = findRun' s gap len Queue.empty

findRun' :: (Num a, Ord a) => [a] -> a -> a -> Queue.Queue (a, a) -> Maybe a
findRun' [] _ _ _ = Nothing
findRun' (n:_) _ 1 _ = Just n
findRun' (n:ns) gap len q =
  case Queue.dequeue q of
    Nothing -> findRun' ns gap len (Queue.enqueue (n + gap, 2) q)
    Just ((k, l), qs) ->
      case compare k n of
        LT -> findRun' (n:ns) gap len qs
        GT -> findRun' ns gap len (Queue.enqueue (n + gap, 2) q)
        EQ -> if l == len then
            Just (k - gap * (len - 1))
          else
            findRun' ns gap len (Queue.enqueue (k + gap, l + 1) qs)

semiprimes :: [Integer]
semiprimes = genSemiprimes primes Heap.empty

genSemiprimes :: [Integer] -> Heap (Entry Integer [Integer]) -> [Integer]
genSemiprimes [] _ = []
genSemiprimes (p:ps) table =
  case Heap.viewMin table of
    Nothing -> genSemiprimes ps (insertPrime (p:ps) table)
    Just (Heap.Entry n [], t) -> n : genSemiprimes (p:ps) t
    Just (Heap.Entry n (n':ns), _) -> if p * p < n' then
        genSemiprimes ps (insertPrime (p:ps) table)
      else
        n : genSemiprimes (p:ps) (Heap.adjustMin (const (Heap.Entry n' ns)) table)
  where
    insertPrime [] = id
    insertPrime (q:qs) = Heap.insert (Heap.Entry (q * q) (map (q *) qs))
    