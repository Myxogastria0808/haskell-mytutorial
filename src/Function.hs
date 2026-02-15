module Function
  ( function,
    printAdd,
    printFibonacci,
    printEvenOdd,
    printFactorial,
    printPower,
    printPowerWithGuard,
    printLambdaAdd,
    printDouble,
    printMap,
    printTwice,
    printFunctionComposition,
  )
where

{-
型クラスに関するわかりやすい記事
https://zenn.dev/airiswim/articles/90456e4768ca03

型クラスは、Rustのトレイト境界に似ている
-}

-- 標準で定義されている even, odd を隠すおまじない
import Prelude hiding (even, odd)

function :: IO ()
function = putStrLn "-- Function --"

-- 関数
{-
- 関数定義は、一般に以下のように書く
関数名 引数1 ... 引数n = 式
- 関数適用
関数名 引数1 ... 引数n
-}

-- 関数の型定義は、以下のようにイメージすれば良い
-- 関数名 :: (型クラス 型1, ...) => 引数1の型 -> ... -> 戻り値の型
-- 型クラスは、Rustのトレイト境界に似ている
add :: (Num a) => a -> a -> a
add x y = x + y

printAdd :: IO ()
printAdd = do
  -- 型定義の型変数で行った場合、具体的な型がわからないというWarningが出る
  -- そのため、具体的な型を指定する
  print (add (1 :: Int) (2 :: Int))
  -- 関数適用は、どの中置記演算子よりも優先される
  -- (= (add 1 2) * 3)
  print (add (1 :: Int) (2 :: Int) * 3)
  -- `$`は関数適用の区切りとして、カッコの代わりに利用できる。
  -- `$`演算子は、最も優先順位の低い右結合の中置演算子である。
  -- 第n引数と第n+1引数の間に置いて、引数の値の範囲を明示する。
  -- add 第1引数 $ 第2引数 <- このように使う
  print (add (3 :: Int) $ add (1 :: Int) $ (2 :: Int) * (2 :: Int))
  -- これは、以下と等しい
  -- ↓ 下記の式を見ると明らかであるが、`$`は`*`よりも優先されるため、`$`の右側の式全体が第2引数になる。
  print (add (3 :: Int) (add (1 :: Int) ((2 :: Int) * (2 :: Int))))

-- 再帰関数
-- Haskellには、ループ構文がないため、再帰関数でループを表現する
-- 例えば、フィボナッチ数列は、以下のように定義される
fibonacci :: (Ord t, Num t) => t -> t
fibonacci x =
  if x <= 1
    then x
    else fibonacci (x - 1) + fibonacci (x - 2)

-- 以下のように、case式を利用して書くこともできる
{-
fibonacci' :: (Ord t, Num t) => t -> t
fibonacci' x =
  case x <= 1 of
    True -> x
    False -> fibonacci' (x - 1) + fibonacci' (x - 2)
-}

printFibonacci :: IO ()
printFibonacci = do
  print (fibonacci (10 :: Int))

-- 相互再帰関数
even :: (Eq a, Num a) => a -> Bool
even x = x == 0 || odd (x - 1)

odd :: (Eq a, Num a) => a -> Bool
odd x = x /= 0 && even (x - 1)

printEvenOdd :: IO ()
printEvenOdd = do
  print (even (10 :: Int)) -- True
  print (odd (10 :: Int)) -- False
  print (even (11 :: Int)) -- False
  print (odd (11 :: Int)) -- True

-- パターンマッチング (関数定義)
-- case式と同様に、関数定義でパターンマッチングを行う場合も、
-- パターンの順番には意味があり、上から順にマッチングが試みられる。
-- 引数が一つの場合
-- ex) 階乗を計算する関数
factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1
factorial x = x * factorial (x - 1)

printFactorial :: IO ()
printFactorial = do
  print (factorial (5 :: Int)) -- output: 120 (= 5!)

-- 引数が複数の場合
-- ex) 累乗を計算する関数 (y^x)
power :: (Eq t1, Fractional t2, Num t1) => t1 -> t2 -> t2
power 0 _ = 1.0
power x y = y * power (x - 1) y

printPower :: IO ()
printPower = do
  print (power (3 :: Int) (2.0 :: Double)) -- output: 8.0 (= 2^3)

-- ガード (関数定義)
-- case式と同様に、関数定義でガードを利用できる
-- ex) 負の指数にも対応した累乗を計算する関数 (y^x)
powerWithGuard :: (Fractional t2, Ord t1, Num t1) => t1 -> t2 -> t2
powerWithGuard 0 _ = 1.0
powerWithGuard x y
  | x > 0 = y * powerWithGuard (x - 1) y
  | otherwise = 1.0 / powerWithGuard (-x) y

printPowerWithGuard :: IO ()
printPowerWithGuard = do
  print (powerWithGuard (3 :: Int) (2.0 :: Double)) -- output: 8.0 (= 2^3)
  print (powerWithGuard (-3 :: Int) (2.0 :: Double)) -- output: 0.125 (= 2^-3)

-- 関数式
-- ラムダ抽象 (lambda abstraction)、無名関数 (unnamed function)、匿名関数 (anonymous function) などとも呼ばれる
{-
\引数1 引数2 ... 引数n -> 定義

始めの\は、ラムダ計算の λ を表す。
-}
-- 以下の関数式は、関数と本質的に等価であり、
-- むしろ理論の面から見れば、関数式の方が関数定義よりも基本的な概念である。
lambdaAdd :: Integer -> Integer -> Integer
lambdaAdd = \x y -> x + y

-- 以下のような表現でも同じ意味になる。
-- カリー化の表現をより明示的に記述できる点において、こちらの方がわかりやすいかもしれない。
lambdaAdd' :: Integer -> Integer -> Integer
lambdaAdd' = \x -> \y -> x + y

-- 関数で記述した場合は、以下の通りである。
functionAdd :: Integer -> Integer -> Integer
functionAdd x y = x + y

printLambdaAdd :: IO ()
printLambdaAdd = do
  print (lambdaAdd (1 :: Integer) (2 :: Integer)) -- output: 3
  print (lambdaAdd' (1 :: Integer) (2 :: Integer)) -- output: 3
  print (functionAdd (1 :: Integer) (2 :: Integer)) -- output: 3

-- 関数の部分適用 (partial application)
-- mult関数のように部分適用に利用できる関数を
-- カリー化された関数 (curried function) と呼ぶ。
mult :: (Num a) => a -> a -> a
mult x y = x * y

-- カリー化されていない関数は、以下のように定義できる
-- 型からもわかるように、タプルを引数に取る
mult' :: (Num a) => (a, a) -> a
mult' (x, y) = x * y

-- double関数は、mult関数を部分適用している。
double :: Int -> Int
double = mult 2

-- わかりやすく表現すると、以下のように記述できる。
double' :: Int -> Int
double' x = mult 2 x

{-
{-関数適用の結合-}
関数適用は左結合であるため、multは以下のように評価される。
mult 2 3 = (mult 2) 3
なので、double 3 は、(mult 2) 3 と等価であり、結果は 6 になる。

{-型における関数適用の結合-}
関数の型定義においては、右結合であるため、multの型は以下のように評価される。
Int -> Int -> Int = Int -> (Int -> Int)
なので、mult 2 の型は、Int -> Int になる。

{-豆知識-}
カリー化 の名前の由来は、Haskell Curryという数学者の名前から来ている。
ちなみに、Haskell言語の名前も、Haskell Curryに由来している。
-}

printDouble :: IO ()
printDouble = do
  print (double (3 :: Int)) -- output: 6
  print (mult' (2 :: Int, 3 :: Int)) -- output: 6
  print (double' (3 :: Int)) -- output: 6

-- 高階関数
-- 関数を引数に取ったり、関数を返り値にしたりする関数
-- ex) map関数
square :: (Num a) => a -> a
square x = x * x

printMap :: IO ()
printMap = do
  -- 関数式のパターン
  print $ map (\x -> x * 2) ([1, 2, 3] :: [Int]) -- output: [2,4,6]
  -- 既存関数のパターン
  print $ map square ([1, 2, 3] :: [Int])

-- 部分適用と高階関数の組み合わせ
-- ex) 2回関数適用する関数 (twice関数)

twice :: (t -> t) -> t -> t
twice f1 x = f1 (f1 x)

printTwice :: IO ()
printTwice = do
  -- (2 * 2) * (2 * 2) = 16
  print $ twice square (2 :: Int) -- output: 16 (= square (square 2))
  -- 部分適用と高階関数の組み合わせ
  -- 関数適用 1回目: (2 * 2) * (2 * 2) = 16 = 2^4
  -- 関数適用 2回目: (16 * 16) * (16 * 16) = 65536 = 2^16
  print $ twice twice square (2 :: Int) -- output: 65536 (= square (square (square (square 2))))

-- 関数合成
-- 関数 `g :: a -> b` と `f :: b -> c`の合成を
-- `f . g`と表現する。 (数学の記法: f ∘ g)
-- `foo x = f(g (h x))`という関数があったとき、以下の3通りの表現ができる。
f :: (Read a) => String -> a
f x = read x

g :: (Show a) => a -> String
g x = show x

h :: Bool -> Int
h x = case x of
  True -> (1 :: Int)
  False -> (0 :: Int)

foo1 :: (Read a) => Bool -> a
foo1 x = f (g (h x))

foo2 :: (Read a) => Bool -> a
foo2 x = (f . g . h) x

-- ポインタフリースタイル
-- 仮引数が現れない関数定義のスタイルのこと
foo3 :: (Read a) => Bool -> a
foo3 = f . g . h

{-
{-関数合成演算子 (.) の定義-}
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)
-}

printFunctionComposition :: IO ()
printFunctionComposition = do
  print (foo1 True :: Int) -- output: 1
  print (foo2 True :: Int) -- output: 1
  print (foo3 True :: Int) -- output: 1
  print (foo1 False :: Int) -- output: 0
  print (foo2 False :: Int) -- output: 0
  print (foo3 False :: Int) -- output: 0
