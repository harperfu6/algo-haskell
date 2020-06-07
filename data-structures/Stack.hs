module Stack (
    Stack,
    emptyStack,
    singleton,
    push,
    pop,
    top,
    isEmptyStack
) where

-- 新しいデータ型スタックの定義
--- Sという要素（データ構築子）をもつ型Stackを定義する
--- Sはa（任意のデータ型）のリストとして定義される
data Stack a = S [a] deriving Show

-- 空のスタック
emptyStack :: Stack a
emptyStack = S []

-- 要素が１つのスタックを作成
singleton :: a -> Stack a
singleton x = S [x]

-- データの追加
--- Stack型のSかをパターンマッチングして，新たなSを生成
push :: Stack a -> a -> Stack a
push (S xs) x = S (x:xs)

-- データの削除
pop :: Stack a -> (a, Stack a)
pop (S []) = error "Empty Stack"
pop (S (x:xs)) = (x, S xs)

-- データの取得
top :: Stack a -> a
top (S []) = error "Empty Stack"
top (S (x:xs)) = x

-- スタックの空判定
isEmptyStack :: Stack a -> Bool
isEmptyStack (S []) = True
isEmptyStack (S _) = False