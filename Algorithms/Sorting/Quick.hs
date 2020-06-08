-- クイックソート
--- 先頭を枢軸とし大小リストに分ける，という操作を繰り返す
--- O(n*logn)だが，枢軸の選び方によっては最悪O(n^2)になる

module Quick (
    quick_sort
) where

quick_sort :: Ord a => [a] -> [a]
quick_sort [] = []
quick_sort (p:xs) = quick_sort lt ++ [p] ++ quick_sort gt
    where
        lt = [x | x <- xs, x < p]
        gt = [x | x <- xs, x >= p]