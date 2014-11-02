n = a `div` length xs
	where
		a = 10
		xs = [1,2,3,4,5]

qsort2 [] = []
qsort2 (x:xs) = qsort2 larger ++ [x] ++ qsort2 smaller
	where 
		smaller = [a | a <- xs, a <= x]
		larger = [b | b <- xs, b > x]

qsort3 [] = []
qsort3 (x:xs) = reverse (qsort3 smaller ++ [x] ++ qsort3 larger)
	where 
		smaller = [a | a <- xs, a <= x]
		larger = [b | b <- xs, b > x]

qsort4 [] = []
qsort4 xs = qsort4 larger ++ qsort4 smaller ++ [x] 
	where 
		x = minimum xs
		smaller = [a | a <- xs, a <= x]
		larger = [b | b <- xs, b > x]


qsort5 [] = []
qsort5 (x:xs) = reverse (qsort5 smaller) ++ [x] ++ reverse (qsort5 larger)
	where 
		smaller = [a | a <- xs, a <= x]
		larger = [b | b <- xs, b > x]

qsort6 [] = []
qsort6 (x:xs) = qsort6 larger ++ [x] ++ qsort6 smaller
	where 
		larger = [a | a <- xs, a > x || a == x]
		smaller = [b | b <- xs, b < x]

qsort7 [] = []
qsort7 (x:xs) = qsort7 larger ++ [x] ++ qsort7 smaller
	where 
		larger = [a | a <- xs, a < x]
		smaller = [b | b <- xs, b > x]

qsort8 [] = []
qsort8 (x:xs) = reverse (
					reverse (qsort8 smaller) ++ [x] ++ reverse (qsort8 larger)
				)
	where 
		smaller = [a | a <- xs, a <= x]
		larger = [b | b <- xs, b > x]

qsort9 [] = []
qsort9 xs = x : qsort9 larger ++ qsort9 smaller
	where 
		x = maximum xs
		smaller = [a | a <- xs, a < x]
		larger = [b | b <- xs, b >= x]

qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
	where 
		smaller = [a | a <- xs, a < x]
		larger = [b | b <- xs, b > x]

qsort_orig [] = []
qsort_orig (x:xs) = qsort_orig smaller ++ [x] ++ qsort_orig larger
	where 
		smaller = [a | a <- xs, a < x]
		larger = [b | b <- xs, b > x]