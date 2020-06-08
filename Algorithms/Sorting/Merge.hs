-- マージソート
--- リストを分割しソートした上で，そのリストを小さいものからマージしていく
--- O(n*logn)

module Merge (
    merge_sort
) where

merge_list :: Ord a => [a] -> [a] -> [a]
merge_list [] ys = ys
merge_list xs [] = xs
merge_list a@(x:xs) b@(y:ys)
    | x <= y     = x : merge_list xs b
    | otherwise  = y : merge_list a ys

merge_sort :: Ord a => [a] -> [a]
merge_sort xs = iter1 (map (:[]) xs)
    where
        iter1 [x] = x -- 要素数が１つ
        iter1 xs = iter1 (iter2 xs) -- 要素数が1つ以上
        iter2 [] = []
        iter2 [x] = [x]
        iter2 (x:y:zs) = merge_list x y : iter2 zs