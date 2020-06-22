-- 自然数 n を素因数分解する関数 factorization n
--- 素因数分解は素数 2, 3, 5, ... で順番に割り算していけばいいが、いちいち素数を求めるのは大変なので、2 と 3 以上の奇数列で割り算していく
factorization :: Integer -> [(Integer, Integer)]
factorization n = if c > 0 then iter 3 m [(2, c)] else iter 3 n []
    where
        (c, m) = factor n 2 0
        -- n を m で割り切れる回数cとその分だけ割った後に余った数の組み合わせ
        factor n m c = if n `mod` m /= 0 then (c, n) else factor (n `div` m) m (c + 1)
        iter i n a
            | n == 1 = reverse a
            | n < i * i = reverse ((n, 1):a)
            | otherwise =
                let (c, m) = factor n i 0
                in if c == 0 then iter (i + 2) n a else iter (i + 2) m ((i, c):a)

-- 自然数 n の約数の個数を求める関数 divisor_num
divisor_num :: Integer -> Integer
divisor_num n = foldl (\a (_,x) -> a * (x + 1)) 1 (factorization n)

-- 自然数 n の約数の合計値を求める関数 divisor_sum
--- 8 の素因数分解は 2^3 、約数の合計値は 8 + 4 + 2 + 1 = 15
--- 12 は 2^2 * 3 に素因数分解でき、その合計値は (4 + 2 + 1) * (3 + 1) = 28
divisor_sum :: Integer -> Integer
divisor_sum n = foldl (\a x -> a * sigma x) 1 (factorization n)
    where
        sigma (x, c)
            | c == 0 = 1
            | otherwise = (x ^ c) + sigma (x, (c - 1))

-- 自然数 n の約数をリストに格納して返す関数 divisor
--- 12 の約数は 24 = (1, 2, 4) と 3 = (1, 3) から、(1, 2, 4) * 1 と (1, 2, 4) * 3 のすべての要素 (1, 2, 4, 3, 6, 12) にな
divisor :: Integer -> [Integer]
divisor n =
    let 
        ((p1,x1):xs) = factorization n
        sigma p n = map (p^) [0..n]
        product xs ys = [x*y | x <-xs, y <-ys]
    in foldl (\a (p,x) -> product (sigma p x) a) (sigma p1 x1) xs

-- 自然数 n 以下の完全数を求める関数 perfect_number
--- 完全数（かんぜんすう，perfect number）とは、その数自身を除く約数の和が、その数自身と等しい自然数のことである
perfect_number :: Integer -> [Integer]
perfect_number n = filter (\x -> ((divisor_sum x) - x) == x) [2..n]