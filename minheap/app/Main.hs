module Main where

import Data.Maybe
import Control.Monad (foldM)
import Data.Bits
import Control.Arrow (first, second)
import Data.List (unfoldr)
import Debug.Trace


data Tree a = Tree { root :: Node a
                   , size :: Int
                   } deriving (Show)

data Node a = NodeValue a (Node a) (Node a) | Empty
              deriving (Show)


data Direction = L | R deriving (Show)

data Crumb a = Crumb Direction a (Node a) deriving (Show)

-- Ref: http://learnyouahaskell.com/zippers
type Breadcrumbs a = [Crumb a]
type TreeZipper a = (Node a, Breadcrumbs a)

main :: IO ()
main = do
    let tree = foldr insert empty [5, 7, 5, 32, 1, -12, 0, 0, 3] :: Tree Int
    print $ unfoldr extract tree

    return ()

empty :: Tree a
empty = Tree { root = Empty, size = 0 }

mapLeft :: (Node a -> Node a) -> Node a -> Node a
mapLeft _ Empty             = Empty
mapLeft f (NodeValue v l r) = NodeValue v (f l) r

mapRight :: (Node a -> Node a) -> Node a -> Node a
mapRight _ Empty             = Empty
mapRight f (NodeValue v l r) = NodeValue v l (f r)

toDirections :: (Bits a, Num a) => a -> [Direction]
toDirections = unfoldr unfolder
    where unfolder 0 = Nothing
          unfolder x = Just (fromBit (x .&. 1), x `shiftR` 1)
          fromBit x = case x of
                        0 -> L
                        1 -> R
                        _ -> error "Unknown direction"

getMax :: Tree a -> Maybe a
getMax Tree { root = Empty }           = Nothing
getMax Tree { root = NodeValue v _ _ } = Just v

-- Refactor this to use a zipper
getLast :: (Bits a, Num a) => Tree a -> Maybe (a, Tree a)
getLast Tree { root = Empty }       = Nothing
getLast Tree { root = n, size = s } = fmap (\(v, n) -> (v, Tree { root = n, size = s - 1 })) . traverse pathBits $ n
    where pathBits = tail . reverse . toDirections $ s
          traverse p Empty               = Nothing
          traverse p n@(NodeValue v l r) = case p of
                                       []     -> Just (v, Empty)
                                       (R:xs) -> fmap (second (\x -> mapRight (const x) n)) . traverse (tail p) $ r
                                       (L:xs) -> fmap (second (\x -> mapLeft (const x) n)) . traverse (tail p) $ l

goLeft :: TreeZipper a -> Maybe (TreeZipper a)
goLeft (Empty, bs)           = Nothing
goLeft (NodeValue v l r, bs) = Just (l, Crumb L v r:bs)

goRight :: TreeZipper a -> Maybe (TreeZipper a)
goRight (Empty, bs)           = Nothing
goRight (NodeValue v l r, bs) = Just (r, Crumb R v l:bs)

goUp :: TreeZipper a -> Maybe (TreeZipper a)
goUp (_, [])              = Nothing
goUp (n, Crumb d v on:cs) = case d of
                             L -> Just (NodeValue v n on, cs)
                             R -> Just (NodeValue v on n, cs)

traverseDown :: Direction -> TreeZipper a -> Maybe (TreeZipper a)
traverseDown L = goLeft
traverseDown R = goRight

insert :: (Ord a) => a -> Tree a -> Tree a
insert v Tree { root = Empty }       = Tree { root = NodeValue v Empty Empty, size = 1 }
insert v Tree { root = r, size = s } = case insertionFocus of
                                         Nothing -> error "Something happened while inserting"
                                         Just z  -> Tree { root = fst (heapifyUp z), size = s + 1 }
    where path           = tail . reverse . toDirections . (+ 1) $ s
          insertionFocus = fmap (first (const (NodeValue v Empty Empty))) . foldM (flip traverseDown) (r, []) $ path

extract :: (Num a, Bits a, Ord a) => Tree a -> Maybe (a, Tree a)
extract Tree { root = Empty } = Nothing
extract t = case getLast t of
              Nothing                                             -> Nothing
              Just (x, rt@Tree { root = Empty })                  -> Just (x, rt)
              Just (x, Tree { root = NodeValue v l r, size = s }) -> let leafZipper = heapifyDown (NodeValue x l r, [])
                                                                         (tree, _) =
                                                                            last
                                                                            . catMaybes
                                                                            . takeWhile isJust
                                                                            . iterate (>>= goUp)
                                                                            $ Just leafZipper
                                                                         newTree = Tree { root = tree, size = s }
                                                                     in Just (v, newTree)


heapifyUp :: (Ord a) => TreeZipper a -> TreeZipper a
heapifyUp z@(_, [])    = z
heapifyUp z@(Empty, _) = heapifyUp . fromJust . goUp $ z
heapifyUp z@(n@(NodeValue v l r), Crumb d pv on:cs)
    | v > pv    = heapifyUp . fromJust . goUp $ (NodeValue pv l r, Crumb d v on:cs)
    | otherwise = heapifyUp . fromJust . goUp $ z


heapifyDown :: (Ord a) => TreeZipper a -> TreeZipper a
heapifyDown z@(Empty, [])                              = z
heapifyDown z@(Empty, _)                               = z
heapifyDown z@(NodeValue v Empty Empty, cs)            = z
heapifyDown z@(NodeValue v (NodeValue lv ll lr) Empty, cs)
    | v < lv    = heapifyDown . fromJust . goLeft $ (NodeValue lv (NodeValue v ll lr) Empty, cs)
    | otherwise = z
heapifyDown z@(NodeValue v Empty (NodeValue rv rl rr), cs) -- This is unlikely to happen unless we have a broken tree
    | v < rv    = heapifyDown . fromJust . goRight $ (NodeValue rv Empty (NodeValue v rl rr), cs)
    | otherwise = z
heapifyDown z@(NodeValue v l@(NodeValue lv ll lr) r@(NodeValue rv rl rr), cs)
    | v < lv && lv >= rv = heapifyDown . fromJust . goLeft $ (NodeValue lv (NodeValue v ll lr) r, cs)
    | v < rv && rv > lv = heapifyDown . fromJust . goRight $ (NodeValue rv l (NodeValue v rl rr), cs)
    | otherwise         = z
