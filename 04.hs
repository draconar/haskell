--pyths1 n = [(x, y, z ) | x <- [1..n], y <- [1..x], z <- [1..n], x^2+y^2==z^2] 
--pyths2 n = [(x, y, z ) | x <- [1..n], y <- [x..n], z <- [y..n], x^2+y^2==z^2] 
--pyths3 n = [(x, y, z ) | x <- [1..n], y <- [1..n], z <- [1..n], x^2+y^2==z^2] 
--pyths4 n = [(x, y (x^2 + y^2)) | x <- [1..n], y <- [1..n]] 

factors n = [x | x <- [1..n], n `mod` x == 0]

perfects n = [x | x <- [1..n], isPerfect x]
	where isPerfect num = sum (factors num) == num

lc = [(x,y) | x <- [1,2,3], y <- [4,5,6]]

lc1 = [z | z <- [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]]
--lc3 = concat [(x,y) | y <= [4,5,6]] | x <-[1,2,3]
lc4 = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

--exercise 5
find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions x xs = find x (zip xs [0..n])
	where n = length xs - 1

scalarprod xs ys = sum [x*y | x <- xs, y <- ys]
scalarprod1 xs ys = sum [x*y | (x,y) <- xs `zip` ys]
scalarprod2 xs ys = product (zipWith (+) xs ys)
