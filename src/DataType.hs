module DataType (dataType) where

dataType :: IO ()
dataType = putStrLn "-- DataType --"

-- データ型
{-
Bool ... 論理型
ex) True, False

Int ... 固定長整数型
ex) 123, 0o12 0x1a

Integer ... 多倍長整数型
ex) 132, 0o12, 0x1a

Float ... 単精度浮動小数点型
ex) 123, 0o12, 0x1a, 3.14. 1.23E-10

Double ... 倍精度浮動小数点型
ex) 123, 0o12, 0x1a, 3.14. 1.23E-10

Char ... 文字型
ex) 'a', 'あ', '\n', '\x3042'

String ... 文字列型 ([Char]のシノニム)
ex1) "hello", "こんにちは\n"
ex2) 以下のようにバックスラッシュで文字列を分割できる (文字列としては連結される)
"Hello, \
\World"
[Char] のシノニムなので 、 リストの操作がそのまま使える
['h', 'e', 'l', 'l', 'o'] <=同値=> "hello"
ex) head "hello" ... 'h'
    tail "hello" ... "ello"
    length "hello" ... 5

(a1, ..., an) ... タブル型 (順序はある)
型の違うものを複数まとめて扱う
ex) (1, 2), (True, "hello", 3.14)
要素が1つのタプルは存在しない (要素が1つならその要素そのものになる)
ex) (123) ... Int型

() ... ユニット型 (何でもない型)

[a] ... リスト型
同じ型のものを複数まとめて扱う
ex) [1, 2, 3], ["hello", "world"], []

Maybe a ... Maybe型 (Option型に相当)
Just a ... aという値が存在する
Nothing ... 値がないことを表す
ex) Just 123, Nothing
-}

-- 論理型
{-
not x ... xの否定
x && y ... xかつy
x || y ... xまたはy

{- 値の透過性 -}
※Haskellでは、参照の透過性は扱わない。
ある式を、その式と同じ値を持つ別の式に置き換えても、
プログラムの意味が変わらないことを指す。 (値でしか扱わないため)

x == y ... xとyが等しい (等価)
x /= y ... xとyが等しくない (非等価)
※非等値を表現する記法が、!=ではなく、/=であることに注意
x > y ... xはyより大きい
x < y ... xはyより小さい
x >= y ... xはy以上
x <= y ... xはy以下
-}
