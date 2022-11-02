module Main where

import Data.Maybe (isJust, fromJust)
import Data.Bits ((.&.), (.|.), shift, shiftR)
import Debug.Trace (traceShowId)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
data MinHeap a = MinHeap (Tree a) Int deriving (Show)

type Breadcrumbs a = [Crumb a]
type Zipper a = (Tree a, Breadcrumbs a)

data Bit = One | Zero deriving (Show)


main :: IO ()
main = do
    let tree = Node 3 (Node 4 (Node 6 Empty Empty) Empty) (Node 7 Empty Empty)
    let heap = MinHeap tree 4

    print $ insert heap 3.5

    return ()


goUp :: Zipper a -> Maybe (Zipper a)
goUp (_, [])                = Nothing
goUp (n, LeftCrumb v l:xs)  = Just (Node v l n, xs)
goUp (n, RightCrumb v r:xs) = Just (Node v n r, xs)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Empty, _)       = Nothing
goLeft (Node v l r, cs) = Just (l, RightCrumb v r:cs)

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Empty, _)       = Nothing
goRight (Node v l r, cs) = Just (r, LeftCrumb v l:cs)

goToRoot :: Zipper a -> Maybe (Zipper a)
goToRoot z@(Empty, []) = Just z
goToRoot z             = last . takeWhile isJust . iterate (>>= goUp) $ Just z


min :: (Num a) => MinHeap a -> Maybe a
min (MinHeap Empty _)        = Nothing
min (MinHeap (Node v _ _) _) = Just v


toBitsList :: Int -> [Bit]
toBitsList 0 = [Zero]
toBitsList n = reverse (process n)
    where process 0 = []
          process n = let rest = process (shiftR n 1)
                      in case n .&. 1 of 0 -> Zero:rest
                                         _ -> One:rest


goToLastNode :: MinHeap a -> Zipper a
goToLastNode (MinHeap Empty _) = (Empty, [])
goToLastNode (MinHeap n size)  = traverse (n, []) path
    where path              = traceShowId $ tail (toBitsList size)
          traverse n []     = n
          traverse n [Zero] = fromJust . goLeft $ n
          traverse n [One]  = fromJust . goRight $ n
          traverse n (b:bs) = traverse (traverse n [b]) bs


heapifyUp :: (Ord a) => Zipper a -> Zipper a
heapifyUp z@(_, [])            = z
heapifyUp z@(Empty, cs)        = heapifyUp . fromJust . goUp $ z
heapifyUp z@(Node v _ _, c:cs) = if shouldBeSwapped v (getCrumbValue c)
                                     then heapifyUp . fromJust . goUp . swap $ z
                                     else heapifyUp . fromJust . goUp $ z
    where shouldBeSwapped cv pv = cv < pv
          swap z@(Empty, _) = z
          swap z@(_, [])    = z
          swap (Node v l r, (LeftCrumb cv ct):cs) = (Node cv l r, LeftCrumb v ct:cs)
          swap (Node v l r, (RightCrumb cv ct):cs) = (Node cv l r, RightCrumb v ct:cs)
          getCrumbValue (LeftCrumb v _) = v
          getCrumbValue (RightCrumb v _) = v

view :: MinHeap a -> Maybe a
view (MinHeap Empty

insert :: (Num a, Show a, Ord a) => MinHeap a -> a -> MinHeap a
insert (MinHeap Empty _) nv = MinHeap (Node nv Empty Empty) 1
insert mh@(MinHeap n s) nv  = MinHeap nt (s + 1)
    where (ln, cs) = traceShowId $ goToLastNode (MinHeap n (s + 1))
          (nt, _) = heapifyUp (Node nv Empty Empty, cs)
