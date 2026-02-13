module List
  ( list,
    defineList,
    printOriginalMap,
    printListFold,
    printNumberSequence,
    printQsort,
  )
where

import Prelude hiding (map)

list :: IO ()
list = putStrLn "-- List --"

-- リスト
-- 値をもつ単位 (LISPでは、"cons"セルとよぶ) を一列に並べた構造となっている。
-- 最後は空リスト (nil) で終わる。
{-
[1, 2, 3]を表すリストは、以下のような構造になる。

  +--------+    +--------+   +---------+
  |   |    |    |   |    |    |   |    |
  | 1 | *--+--->| 2 | *--+--->| 3 | *--+---> nil
  |   |    |    |   |    |    |   |    |
  +--------+    +--------+    +--------+

リストの構造は、"代数的データ型"で自然に表せるので、
配列よりもリストが好んで使われる。

リストは、任意の位置の要素にアクセスするのが遅い (O(n)) という欠点がある。

{-等質的 (homogeneous) なリスト-}
リストの要素は、すべて同じ型でなければならない。
-}

-- リストの定義は2通りある。
defineList :: IO ()
defineList = do
  let -- cons構成子による書き方
      -- cons構成子は、以下のように定義された右結合の演算子。
      -- (:) :: a -> [a] -> [a]
      -- x : xs は、xがリストの先頭要素、xsが残りのリストを表す。
      list1 = 1 : 2 : 3 : []
      -- リストリテラルによる書き方
      list2 = [1, 2, 3]
   in do
        -- リストの型定義は [Int] のように書く。
        print (list1 :: [Int])
        -- 型定義は [] Int でも同じ意味。
        print (list2 :: [] Int)

-- リストを扱う関数
-- ex) p関数を自作する
map :: (t -> a) -> [t] -> [a]
map _ [] = []
map f (x : xs) = f x : map f xs

printOriginalMap :: IO ()
printOriginalMap = do
  print ((map (\x -> x * x) [1, 2, 3]) :: [Int]) -- output: [1,4,9]

-- リストの畳み込み
{-
リストの畳み込みには、foldr 関数とfoldl 関数を使う。
foldr ... 二項演算子を使って右からリストを畳み込む関数
foldl ... 二項演算子を使って左からリストを畳み込む関数

ex) リストの和を求める
-- 右から畳み込む
foldr (+) 0 [1, 2, 3] -- 1 + (2 + (3 + 0)) = 6
-- 左から畳み込む
foldl (+) 0 [1, 2, 3] -- ((0 + 1) + 2) + 3 = 6

{-foldrについて-}
リスト [1, 2, 3] が (1 : (2 : (3 : []))) という構造であることを思い出すと、
foldr (+) 0 [1, 2, 3] は二項演算子を : から + に、
最後の [] を 0 に置き換えたものと考えられる。
-}

printListFold :: IO ()
printListFold = do
  print (foldr (+) 0 [1, 2, 3] :: Int)
  print (foldl (+) 0 [1, 2, 3] :: Int)

-- 数列記法
-- リストを [m..n]と書くことで、リスト [m, m+1, ..., n-1, n] を表現できる記法。
printNumberSequence :: IO ()
printNumberSequence = do
  -- パターン1
  print ([1 .. 5] :: [Int]) -- output: [1,2,3,4,5]
  -- パターン2
  -- 始めに2つの要素を指定することで、等差数列の差のステップを指定できる。
  print ([1, 3 .. 9] :: [Int]) -- output: [1,3,5,7,9]
  -- パターン3
  -- takeで要素数を指定できる
  print (take 5 [1 ..] :: [Int]) -- output: [1,2,3,4,5]
  -- 無限リスト
  -- Haskellでは、遅延評価を採用しているために
  -- 無限リストを扱うことができる。
  -- ※終わらないので、コメントアウトしている。
  -- print ([1 ..] :: [Int]) -- output: [1,2,3,...] (無限に続く)

-- リスト内包表記 (list comprehension)
{-
[ 式 | 限定子1, 限定子2, ..., 限定子n ]

{-限定子の種類-}
- ジェネレータ ... `パターン <- 式(リスト)` と書く
ex) [ x | x <- [1, 2, 3] ] -- output: [1,2,3]
- ガード ... `条件式` を書く
ex) [ x | x <- [1, 2, 3], odd x ] -- output: [1,3]
局所宣言 ... `let`で始まる宣言を書く
ex) [(x, y) | x <- [1, 2, 3], let y = 4] -- output: [(1,4),(2,4),(3,4)]

{-リスト内包表記でmapを定義する-}
map f xs = [f x | x <- xs]
-}

{-クイックソートを実装する-}
{-
pivotを境界にして、
pivotより小さい要素のリスト (smaller) と
pivot以上の要素のリスト (bigger) に分割し、
それぞれを再帰的にクイックソートしたものを結合する
-}
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (pivot : xs) =
  let smaller = [x | x <- xs, x < pivot]
      bigger = [x | x <- xs, x >= pivot]
   in qsort smaller ++ [pivot] ++ qsort bigger

printQsort :: IO ()
printQsort = do
  {-
  クイックソートの実行例
  ex) qsort [3, 2, 4, 1, 5]

  1. [2, 1] ++ [3] ++ [4, 5]
    1-1. qsort [2, 1] (qsort smaller)について
    - [1] ++ [2] ++ []
      1-1-1. qsort [1] (qsort smaller)について
        - [] ++ [1] ++ []
        1-1-1-1. qsort [] (qsort smaller)について
          - []
        1-1-1-2. qsort [] (qsort bigger)について
          - []
      1-1-2. qsort [] (qsort bigger)について
        - []
      結果として、[1, 2] になる。
    1-2. qsort [4, 5] (qsort bigger)について
    - [] ++ [4] ++ [5]
      1-2-1. qsort [5] (qsort bigger)について
        - [] ++ [5] ++ []
        1-2-1-1. qsort [] (qsort smaller)について
          - []
        1-2-1-2. qsort [] (qsort bigger)について
          - []
      1-3-2. qsort [] (qsort smaller)について
        - []
      結果として、[4, 5] になる。
  2. 結果として、[1, 2] ++ [3] ++ [4, 5] になる。
  3. 最終的に、[1, 2, 3, 4, 5] になる。
  -}
  print (qsort [3, 2, 4, 1, 5] :: [Int]) -- output: [1,2,3,4,5]
