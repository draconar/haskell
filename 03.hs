halve xs = (take n xs, drop n xs)
	where n = length xs / 2

halve1 xs = splitAt (length xs `div` 2) xs

halve2 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
	where n = length xs

halve3 xs = splitAt (length xs `div` 2)

halve4 xs = (take n xs, drop (n + 1) xs)
	where n = length xs `div` 2

halve5 xs = splitAt (div (length xs) 2) xs)

halve6 xs = splitAt (length xs / 2)

halve7 xs = (take n xs, drop n xs)
	where n = length xs `div` 2


safetail xs = if null xs then [] else tail xs

safetail2 [] = []
safetail2 (_ : xs) = xs

safetail3 (_ : xs)
	| null xs = []
	| otherwise = tail xs

safetail4 xs
	| null xs = []
	| otherwise = tail xs

safetail5 xs = tail xs
safetail5 [] = []

safetail6 [] = []
safetail6 xs = tail xs

safetail7 [x] = [x]
safetail7 (_:xs) = xs

safetail8 = \ xs -> 
				case xs of
					[] -> []
					(_:xs) -> xs

import Prelude hiding ((||))
False || False = False
_ || _ = True

False || b = b
True || _ = True

b || c
	| b == c = b
	| otherwise = True

b || False = b
_ || True = True

b || c 
	| b == c = c
	| otherwise = True

b || True = b
_ || True = True

False || False = False
False || True = True
True || False = True
True || True = True


import Prelude hiding ((&&))
True && True = True
_ && _ = False

a && b = if a then if b then True else False else False

a && b = if not (a) then not (b) else True

a && b = if a then b

a && b = if a then if b then False else True else False

a && b = if a then b else False

a && b = if b then a else False


mult x y z = \ x -> ( \ y -> (\z -> x * y * z))

mult = \x -> (x * \y -> (y * \z -> z))

mult = \x -> (\y -> (\z -> x * y * z))

mult = ((((\x -> \y) -> \z) -> x * y) * z)