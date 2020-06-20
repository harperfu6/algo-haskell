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

-- リストそのものを関数に渡して畳み込みを行う方法も考えられます。リストの先頭から畳み込みを行う関数 pair_foldl と、末尾から畳み込みを行う関数 pair_foldr
pair_foldl :: ([a] -> b -> b) -> b -> [a] -> b
pair_foldl f ax m@(x:xs) = pair_foldl f (f m ax) xs
pair_foldl _ ax _ = ax

pair_foldr :: ([a] -> b -> b) -> b -> [a] -> b
pair_foldr f ax m@(x:xs) = f m (pair_foldr f ax xs)
pair_foldr _ ax _ = ax

-- リストを平坦化する関数 flatten
flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

-- リスト xs に格納されたリストに関数 f oを適用し、その結果を連結する関数 flatmap f xs
flatmap :: (a -> [b]) -> [a] -> [b]
flatmap _ [] = []
flatmap f (x:xs) = f x ++ flatmap f xs

-- 集合を表すリスト xs, ys の直積集合を求める関数 product_set xs ys 
product_set :: [a] -> [b] -> [(a, b)]
product_set xs ys = [(x, y) | x <- xs, y <- ys]

-- リスト xs のべき集合を求める関数 power_set xs
power_set :: [a] -> [[a]]
power_set [] = [[]]
power_set (x:xs) = power_set xs ++ [x:ys | ys <- power_set xs]

-- リスト xs に x を挿入するパターンをすべて求めてリストに格納して返す関数 interleave x xs 
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x a@(y:ys) = [x:a] ++ map (y:) (interleave x ys)

-- リストから n 個の要素を選ぶ順列を求める関数 permutation
permutation :: Eq a => Int -> [a] -> [[a]]
permutation 0 _ = [[]]
permutation n xs = [x:ys | x <- xs, ys <- permutation (n - 1) (remove x xs)]

-- リストからすべての要素を選ぶ順列を求める関数 permutation1
permutation1 :: [a] -> [[a]]
permutation1 [] = [[]]
permutation1 (x:xs) = flatmap (interleave x) (permutation1 xs)

-- リストから重複を許して n 個の要素を選ぶ順列を求める関数 repeat_perm
-- repeat_perm :: Int -> [a] -> [[a]]
repeat_perm :: Int -> [a] -> [[a]]
repeat_perm 0 _ = [[]]
repeat_perm n xs = [x:ys | x <- xs, ys <- repeat_perm (n - 1) xs]

-- リストから n 個の要素を選ぶ組み合わせを求める関数 combination
combination :: Int -> [a] -> [[a]]
combination 0 _ = [[]]
combination _ [] = []
combination n (x:xs) = [x:y | y <- (combination (n - 1) xs)] ++ combination n xs
-- combination n (x:xs) = map (x:) (combination (n - 1) xs) ++ combination n xs

-- リストから重複を許して n 個の要素を選ぶ組み合わせを求める関数 repeat_comb
repeat_comb :: Int -> [a] -> [[a]]
repeat_comb 0 _ = [[]]
repeat_comb _ [] = error "repeat_comb empty list"
repeat_comb n [x] = [make_list x n]
repeat_comb n a@(x:xs) =
  [x:y | y <- repeat_comb (n - 1) a] ++ repeat_comb n xs

-- リストを n 番目の要素で二分割する関数 split_at
split_at :: Int -> [a] -> ([a], [a])
split_at n xs = (take n xs, drop n xs)

-- リストの要素に述語 p を適用し、一つでも真を返す要素があれば真を返す関数 any と、一つでも偽を返す要素があれば偽を返す (全てが真の場合に真を返す) 関数 every
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs)
    | p x = True
    | otherwise = any' p xs

every :: (a -> Bool) -> [a] -> Bool
every _ [] = True
every p (x:xs)
    | p x = every p xs
    | otherwise = False

-- y と等しいリスト xs の要素を全て x に置換する関数 substitute x y xs
substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute x y (n:xs) = (check x y n) : substitute x y xs
    where
        check x y n
            | y == n = x
            | otherwise = n

-- 述語 p が真を返す要素を全て x に置換する関数 substitute_if p x xs
substitute_if :: (a -> Bool) -> a -> [a] -> [a]
substitute_if _ _ [] = []
substitute_if p x (n:xs) = (check p x n) : substitute_if p x xs
    where
        check p x n
            | p n = x
            | otherwise = n

-- リスト xs の中で連続した等しい要素を部分リストにまとめる関数 pack
pack :: Eq a => [a] -> [[a]]
pack (x:xs) = iter xs [x] [] -- xsは残りのリスト，[x]は今対象のリスト，[]はチェック済みのリスト
    where
        iter [] ys zs = reverse (ys:zs) -- チェック済みはとりあえず蓄積して最後反転
        iter (x:xs) ys@(y:_) zs
            | x == y = iter xs (x:ys) zs
            | otherwise = iter xs [x] (ys:zs)

-- 整列済みの整数を表すリストで、連続している部分列を (start, end) に置き換える関数 pack_num_list
pack_num_list :: [Integer] -> [(Integer, Integer)]
pack_num_list (x:xs) = iter xs [(x, x)]
    where
        iter [] a = reverse a
        iter (x:xs) a@((s, e):ys)
            | x == e + 1 = iter xs ((s, x):ys)
            | otherwise = iter xs ((x, x):a)

-- 上記の逆変換を行う関数 expand_num_list
expand_num_list :: [(Integer, Integer)] -> [Integer]
expand_num_list [] = []
expand_num_list (x:xs) = expand x ++ expand_num_list xs
    where
        expand (s, e)
            | s == e = [e]
            | otherwise = s : expand ((s + 1), e)

-- 連続している同じ記号を (code, num) に変換する関数 encode
encode :: Eq a => [a] -> [(a, Int)]
encode (x:xs) = iter xs [(x, 1)]
    where
        iter [] a = reverse a
        iter (x:xs) a@((c, n):ys)
            | x == c = iter xs ((c, (n + 1)):ys)
            | otherwise = iter xs ((x, 1):a)

-- 上記の逆変換を行う関数 decode
decode :: Eq a => [(a, Int)] -> [a]
decode [] = []
decode (x:xs) = expand x ++ decode xs
    where
        expand (c, n)
            | n == 0 = []
            | otherwise = c : expand (c, (n - 1))

-- 自然数 n 以下の素数をすべて求める関数 sieve
sieve :: Integer -> [Integer]
sieve n = foldr (\x a -> (iter x 2)++a) [] [2..n]
    where
        iter n c
            | (c * 2) > n = [n]
            | n `mod` c == 0 = [] 
            | otherwise = iter n (c + 1)