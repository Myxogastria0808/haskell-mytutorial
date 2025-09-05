module Main (main) where

import Formula (casePattern, guardPattern, ifPattern)

{-
参考サイト
http://walk.northcol.org/haskell/
-}

main :: IO ()
main = do
  helloWorld
  layoutPattern
  variable
  patternPattern
  printAdd
  casePattern
  guardPattern
  ifPattern

-- IOアクションを返すmain関数
-- ※ 明示的に型宣言を書いているが、推論されるときもある
-- (ただ、書かないとWarningが出る)
helloWorld :: IO ()
-- putStrLnに文字列を適用している
{-
※関数型プログラミングでは、関数を「呼び出す (call)」ではなく、
「適用する (apply)」と言った方が好まれる
-}
helloWorld = putStrLn "Hello, World!"

-- 一行コメントアウト
{-
  複数行コメントアウト
  {- ネストさせることも可能 -}
-}

-- ブロック
-- 複数の式をまとめて1つの式にする構文
{-
ブロックを導入するキーワードとして、
`do`、`where`、`let`、`of` などがある

blockPattern = do { putStrLn "morning"; putStrLn "afternoon"; putStrLn "night" }
{- 一行であれば { } を省略できる -}
blockPattern = do putStrLn "morning"； putStrLn "afternoon"； putStrLn "night"
-}

-- レイアウト
-- ブロックをインデントのレベルで表現する仕組み (Pythonと同じ仕組み)
-- フォーマッタによって、ブロックは自動的にレイアウトに変換される
{-
{- レイアウト規則 (オフサイドルール) -}
複数の式のインデントを揃えることにより、
それらの式が同じブロックに属することを示す規則のこと

インデントの位置は、最初の式の位置で決定される
(自動でフォーマットされる場合は、気にする必要はない)
-}
layoutPattern :: IO ()
layoutPattern = do
  putStrLn "morning"
  putStrLn "afternoon"
  putStrLn "night"

-- 変数
{-
変数の型推論は自動で行われる。
変数に価を結び付けることを、変数の束縛 (binding) という。
※Haskellには、代入の概念はない (変数は不変)
-}
variable :: IO ()
variable = do
  let val :: Int -- 型注釈
      val = 123 -- 変数 val に値 123 を束縛
  print val

-- パターン
-- 変数束縛の方法の一つ
patternPattern :: IO ()
patternPattern = do
  -- パターンによる変数束縛
  let (a, b, c) = (123, 3.14, "hello") :: (Int, Double, String) -- タプルのパターン
  -- ちなみに、以下のようにも書けるが、コンパイル時に右辺の長さが3であることを保証できないので、Warningが出る
  -- let [x, y, z] = [1, 2, 3] -- リストのパターン
  -- ワイルドカード (_) を利用すると、不要な値を無視できる
  let (d, _, _) = (1, 2, 3) :: (Integer, Integer, Integer)
  -- また、値構成子も利用できる
  do
    print a -- 123
    print b -- 3.14
    print c -- "hello"
    print d -- 1

-- 関数
{-
- 関数定義は、一般に以下のように書く
関数名 引数1 ... 引数n = 式
- 関数適用
関数名 引数1 ... 引数n
-}

-- 関数の型定義は、以下のようにイメージすれば良い
-- 関数名 :: (Rustで言うTrait境界1 型1, ...) => 引数1の型 -> ... -> 戻り値の型
add :: (Num a) => a -> a -> a
add x y = x + y

printAdd :: IO ()
printAdd =
  -- 型定義の型変数で行った場合、具体的な型がわからないというWarningが出る
  -- そのため、具体的な型を指定する
  print (add (1 :: Int) (2 :: Int))

-- 識別子
{-
{- 変数識別子 (variable identifier) -}
- 変数識別子は小文字で始まる
- 用途: 変数、関数、型変数
{- 構成子識別子 (constructor identifier) -}
- 構成子識別子は大文字で始まる
- 用途: データ構成子、型構成子、型クラス、モジュール
-}

-- 多相型 (polymorphic type)
{-
JavaやC++のジェネリクスに相当する
{-具体例-}
fst関数 (組の第一要素を取り出す関数)
型としては、以下のようになっている
- a, b は型変数 (type variable) と呼ばれ、任意の型を表す
fst :: (a, b) -> a

実際の利用例
fst (1, 'a') -- output: 1
fst (True, "hello") -- output: True
-}

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
