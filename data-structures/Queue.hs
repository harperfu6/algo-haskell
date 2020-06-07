module Queue (
    Queue,
    emptyQueue,
    singleton,
    enqueue,
    dequeue,
    front,
    isEmptyQueue
 ) where

-- キューの定義
--- front/rear
data Queue a = Q [a] [a] deriving Show

-- 空のキュー
emptyQueue :: Queue a
emptyQueue = Q [] []

-- 要素が１つのキューを取りだす
singleton :: a -> Queue a
singleton x = Q [x] []

-- データの追加
enqueue :: Queue a -> a -> Queue a
enqueue (Q front rear) x = Q front (x:rear)

-- データの取り出し
dequeue :: Queue a -> (a, Queue a)
dequeue (Q [] []) = error "Empty Queue"
dequeue (Q [] rear) = dequeue (Q (reverse rear) [])
dequeue (Q (x:xs) rear) = (x, Q xs rear)

-- 先頭データの参照
front :: Queue a -> a
front (Q [] []) = error "Empty Queue"
front (Q [] rear) = front (Q (reverse rear) [])
front (Q (x:_) _) = x

-- キューは空か
isEmptyQueue :: Queue a -> Bool
isEmptyQueue (Q [] []) = True
isEmptyQueue (Q _ _) = False
