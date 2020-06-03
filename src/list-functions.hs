replicate' :: Int -> a -> [a]
replicate' c x
    | c <= 0    = []
    | otherwise = x:replicate' (c - 1) x

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ []     = []
take' c (x:xs) = x:take' (c - 1) xs


reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' ls = reverseHelper ls []
    where
        reverseHelper [] acc     = acc
        reverseHelper (x:xs) acc = reverseHelper xs (x:acc)

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []     = False
elem' e (x:xs) = if x == e then True else elem' e xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' _ [] = False
elem'' e (x:xs)
    | e == x = True
    | otherwise = elem'' e xs
