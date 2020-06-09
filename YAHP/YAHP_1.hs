module YAHP_1 (
    single
) where

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