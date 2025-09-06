module Operator
  ( operator,
    printOriginalOperator,
    printPreOperator,
    printMidOperator,
    printSection,
  )
where

operator :: IO ()
operator = putStrLn "-- Operator --"

-- 演算子
{-
{-結合性-}
- 左結合： a - b - c は (a - b) - c と同じ。
- 右結合： a ^ b ^ c は a ^ (b ^ c) と同じ。
- 非結合: 結合性に意味を持たない。
    - a > b > c という式は、Haskellでは無効である。
-}

-- 演算子のユーザ定義
{-
演算子で使える記号は、主に以下の通り。
※ Unicodeで記号と定義されている文字も使える。
! # $ % & * + . / < = > ? @ \ ^ | - ~

ただし、以下の記号列は予約されていて、独自に定義できない。
また、コロン (:) で始まる記号列は、構成子用に予約されている。
.. : :: = \ | <- -> @ ~ =>
-}

-- 演算子は2引数の関数として定義する
-- ex) 累乗演算子 (x ^^^ y = x^y)
(^^^) :: (Eq t1, Fractional t2, Num t1) => t2 -> t1 -> t2
_ ^^^ 0 = 1.0
x ^^^ y = x * (x ^^^ (y - 1))

--- 結合性宣言
--- 演算子の結合性と優先順位を指定する
{-
以下のように宣言する
結合性 優先順位 演算子

{-結合性の種類-}
- infixl: 左結合
- infixr: 右結合
- infix: 非結合

{-優先順位の範囲-}
- 0 (最も弱い) から 9 (最も高い) までの整数
- デフォルトの優先順位は、9 である。
-}
infixr 8 ^^^ -- 右結合、優先度8

printOriginalOperator :: IO ()
printOriginalOperator = do
  print (((2 :: Double) ^^^ (3 :: Double)) :: Double) -- output: 8.0 (= 2^3)

-- 中置記法と前置記法の切り替え
{-
中置演算子は、かっこ ( ... ) で囲むことにより、一般の関数と同じように扱える。
-}
printPreOperator :: IO ()
printPreOperator = do
  print (((+) 1 2) :: Int) -- output: 3
  print (((*) 3 4) :: Int) -- output: 12

{-
2引数関数は、バッククォート `...` で囲むことにより、中置演算子として扱える。
-}
printMidOperator :: IO ()
printMidOperator = do
  print ((5 `div` 2) :: Int) -- output: 2
  print ((5 `mod` 2) :: Int) -- output: 1

-- セクション (section)
-- 中置演算子の部分適用をするための記法。
{-
(演算子 <引数>) または (<引数> 演算子)

ex) (+) の演算子のセクション
(x +) -- \y -> x + y
(+ y) -- \x -> x + y

※ ただし、式`(- x)`はセクションとは解釈されずに、
  単項マイナス演算子の適用と解釈される。
  セクションとしての意味を得るには、`(substruct x)` または `(+ (-x))`と書く。
-}
-- ex) (+) の演算子のセクション
printSection :: IO ()
printSection = do
  let add1 = (+ 1)
      add1' = (1 +)
  do
    print ((add1 2) :: Int) -- output: 3
    print ((add1' 2) :: Int) -- output: 3
    -- map (\x -> 2 * x) [1, 2, 3] と等価の式は以下の通り
    print (map (2 *) [1, 2, 3] :: [Int]) -- output: [2,4,6]
