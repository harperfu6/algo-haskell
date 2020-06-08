-- 挿入ソート
--- ソート済みリストに対しリスト１つ１つを走査して大小を比較し適切な位置に挿入
--- O(n^2) 簡単だが遅い

module Insert (
    insert_sort
) where

insert_element :: Ord a => (a, [a]) -> [a]
insert_element (x, []) = [x]
insert_element (x, xs@(y:ys))
    | x <= y = x:xs
    | otherwise = y : insert_element (x, ys)

insert_sort :: Ord a => [a] -> [a]
insert_sort [] = []
insert_sort (x:xs) = insert_element(x, insert_sort xs)