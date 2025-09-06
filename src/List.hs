module List
  ( list,
    defineList,
    printOriginalMap,
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

  +--------+    +--------+    +---------+
  | 1 | *--+--->| 2 | *--+--->| 3 | *--+---> nil
  +--------+    +--------+    +---------+

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
