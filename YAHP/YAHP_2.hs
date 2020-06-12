-- 要素 x を n 個持つリストを生成する関数 make_list x n
make_list :: a -> Int -> [a]
make_list xs 0 = []
make_list xs n = xs : make_list xs (n - 1)

-- 整数 n から m までの値に関数 f を適用した結果をリストに格納して返す関数 tabulate f n m
tabulate :: (Int -> a) -> Int -> Int -> [a]
tabulate f n m = map f [n..m]

-- リスト xs から要素 x を削除する関数 remove x xs と、述語 p が真を返す要素を削除する関数 remove_if p xs
remove :: Eq a => a -> [a] -> [a]
remove n xs = [x | x <- xs, x /= n]

remove_if :: (a -> Bool) -> [a] -> [a]
remove_if p xs = [x | x <- xs, not (p x)]

-- 2 つのリスト xs, ys を受け取り、各々の要素に対して関数 f を適用し、その結果をリストに格納して返すマップ関数 map2 f xs ys
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys
map2 f _ _ = []

-- 2 つのリスト xs, ys を畳み込む関数 foldl2 f a xs ys と foldr2 f a xs ys
foldl2 :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldl2 f a (x:xs) (y:ys) = foldl2 f (f x y a) xs ys
foldl2 _ a _ _ = a 

foldr2 :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldr2 f a (x:xs) (y:ys) = f x y (foldr2 f a xs ys)
foldr2 _ a _ _ = a 

-- 関数 maplist は関数 f にリストそのものを渡します。ただし、繰り返すたびにリストの先頭要素は取り除かれていきます。関数 maplist を定義
maplist :: ([a] -> b) -> [a] -> [b]
maplist _ [] = []
maplist f a@(x:xs) = f a : maplist f xs