-- リストの要素がただひとつか調べる述語 single
single :: [a] -> Bool
single [_] = True
single _ = False

-- リストの要素がひとつ以上あるか調べる述語 pair
pair :: [a] -> Bool
pair (_:_) = True
pair _ = False

-- リスト xs はリスト ys よりも長いか調べる述語 longer
longer :: [a] -> [a] -> Bool
longer [] _ = False
longer _ [] = True
longer (_:xs) (_:ys) = longer xs ys

-- リストの最後尾を求める関数 last_pair と、最後尾の要素を取り除く関数 butlast 
last_pair :: [a] -> [a]
last_pair [] = error "last_pair empty list"
last_pair [x] = [x]
last_pair (_:xs) = last_pair xs

butlast :: [a] -> [a]
butlast [] = error "butlast empty list"
butlast [_] = []
butlast (x:xs) = x : butlast xs

-- リストの先頭から n 個の要素を取り出す関数 take n xs 
take' :: Int -> [a] -> [a]
-- take' 0 _ = [] nに負が渡されると対応できない
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

-- リストの先頭から n 個の要素を取り除く関数 drop n xs
drop' :: Int -> [a] -> [a]
drop' n xs
    | n <= 0 = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n - 1) xs

-- リストの末尾から n 個の要素を取り出す関数 takeR n xs
takeR :: Int -> [a] -> [a]
takeR n xs = reverse $ take' n $ reverse xs

-- リストの末尾から n 個の要素を取り除く関数 dropR n xs
dropR :: Int -> [a] -> [a]
dropR n xs = reverse $ drop' n $ reverse xs

-- リスト xs を長さ n の部分リストに分割する関数 group n xs
group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take' n xs : group n (drop' n xs)

-- 2 つのリスト xs, ys の要素 x, y を取り出し、タプル (x, y) にまとめてリストに格納して返す関数 zip xs ys 
zip1 :: [a] -> [b] -> [(a, b)]
-- zip1 _ [] = []
-- zip1 [] _ = []
zip1 (x:xs) (y:ys) = (x, y) : zip1 xs ys
zip1 _ _ = [] -- どちらかが空はこれでokだが，すべてにマッチングするのでotherwiseのようにする

-- zip したリストを元に戻す関数 unzip xs
unzip1 :: [(a, b)] -> ([a], [b])
unzip1 xs = ([x | (x, _) <- xs], [y | (_, y) <- xs])

-- 対応する値を求める関数 assoc key alist と、述語 pred が真を返すキーを探す関数 assoc_if pred alist
assoc :: Eq a => a -> [(a, b)] -> Maybe b
assoc _ [] = Nothing
assoc n ((a, b):xs)
    | n == a = Just b
    | otherwise = assoc n xs

assoc_if :: (a -> Bool) -> [(a, b)] -> Maybe b
assoc_if _ [] = Nothing
assoc_if f ((a, b):xs)
    | f a = Just b
    | otherwise = assoc_if f xs

-- リスト xs の中から述語 pred が真を返す最初の要素を求める関数 find_if pred xs
find_if :: (a -> Bool) -> [a] -> Maybe a
find_if _ [] = Nothing
find_if f (x:xs)
    | f x = Just x
    | otherwise = find_if f xs

-- リスト xs の中から述語 pred が真を返す最初の要素の位置を求める関数 position_if pred xs
position_if :: (a -> Bool) -> [a] -> Maybe Int
position_if p xs = iter 0 xs
    where
        iter _ [] = Nothing
        iter i (x:xs)
            | p x = Just i
            | otherwise = iter (i + 1) xs

-- リスト xs から述語 pred が真を返す要素の個数を求める関数 count_if pred xs
count_if :: (a -> Bool) -> [a] -> Int
count_if p xs = foldl (\a x -> if p x then a + 1 else a) 0 xs

-- リスト xs の中から最大値を求める関数 max_list xs と最小値を求める関数 min_list xs
max_list :: Ord a => [a] -> a
max_list (x:xs) = foldl (\a x -> if a < x then x else a) x xs

min_list :: Ord a => [a] -> a
min_list (x:xs) = foldl (\a x -> if a > x then x else a) x xs

-- リスト xs から重複要素を取り除く関数 removeDup xs
removeDup :: Eq a => [a] -> [a]
--- リスト要素チェックはelem
removeDup xs = foldr (\x a -> if elem x a then a else x:a) [] xs

-- 2 つの集合の和を求める関数 union xs ys
union :: Eq a => [a] -> [a] -> [a]
union xs ys = removeDup (xs++ys)

-- 2 つの集合の積を求める関数 intersection xs ys
intersection :: Eq a => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, elem x ys]

-- 2 つの集合の差を求める関数 difference xs ys 
difference :: Eq a => [a] -> [a] -> [a]
difference xs ys = [x | x <- xs, notElem x ys]

-- リスト xs を挿入ソートする関数 insert_sort xs
insert_sort :: Ord a => [a] -> [a]
insert_sort [] = []
insert_sort (x:xs) = insert_element x (insert_sort xs)
    where
        insert_element x [] = [x]
        insert_element x a@(y:ys)
            | x > y = y : insert_element x ys
            | otherwise = x : a

-- リスト xs を述語 pred が真を返すものと偽を返すものの 2 つに分ける関数 partition_if pred xs
partition_if :: (a -> Bool) -> [a] -> ([a], [a])
partition_if p xs = ([x | x <- xs, p x], [x | x <- xs, not (p x)])

-- リスト xs をクイックソートする関数 quick_sort xs
quick_sort :: Ord a => [a] -> [a]
quick_sort [] = []
quick_sort (n:xs) = (ql n xs) ++ [n] ++ (qr n xs)
    where
        ql n xs = quick_sort [x | x <- xs, x < n]
        qr n xs = quick_sort [x | x <- xs, x >= n]

-- 2 つのソート済みのリストをひとつのソート済みのリストにまとめる関数 merge_list xs ys
merge_list :: Ord a => [a] -> [a] -> [a]
merge_list xs [] = xs
merge_list [] ys = ys
merge_list a@(x:xs) b@(y:ys)
    | x <= y = x : merge_list xs b
    | otherwise = y : merge_list a ys

-- 関数 merge_list を使って長さ n のリスト xs をソートする merge_sort n xs
merge_sort :: Ord a => [a] -> [a]
merge_sort xs = iter1 (map (:[]) xs)
    where
        iter1 [x] = x -- 最終的にマージを繰り返し[[1,2,3...]]のような状態になったら中身を取り出す
        iter1 xs = iter1 (iter2 xs)
        iter2 [] = []
        iter2 [x] = [x]
        iter2 (x:y:zs) = merge_list x y : iter2 zs