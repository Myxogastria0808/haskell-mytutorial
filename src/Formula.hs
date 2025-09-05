module Formula (casePattern, guardPattern, ifPattern, letArea, whereArea) where

import Prelude hiding (pi)

-- パターンマッチング (case式)
-- 式の値をパターンマッチングする仕組み
{-
以下、レイアウトの表現で書いている
case 式 of
  パターン1 -> 式1
  パターン2 -> 式2
  ...
  パターンn -> 式n

- マッチしたパターンに対応する式がcase式全体の値になる

※ 上から順にパターンに当てはまるかチェックされるので、パターンの順番には意味がある。
従って、以下のようなcase式は、意味がない (パターン1が常にマッチするため)
case 式 of
  _ -> 式1
  パターン2 -> 式2
  ...
  パターンn -> 式n
-}
getValueFn :: p -> Maybe p -> p
getValueFn defaultValue maybeTypeValue =
  case maybeTypeValue of
    Nothing -> defaultValue
    Just v -> v

casePattern :: IO ()
casePattern = do
  print (getValueFn (123 :: Int) Nothing) -- output: 123
  print (getValueFn (123 :: Int) (Just 456)) -- output: 456

-- ガード (case式)
-- パターンマッチングに加えて論理 (条件式) で式の評価を制御する仕組み
-- 以下、レイアウトの表現で書いている
{-
case 式 of
  パターン1  | 条件式1 -> 式1
            | 条件式2 -> 式2
            ...
            | 条件式n -> 式n
  ...
  パターンn
-}
absMaybeFn :: (Num a, Ord a) => Maybe a -> a
absMaybeFn x =
  case x of
    Nothing -> 0
    Just v
      | v < 0 -> -v -- Just v にマッチし、v < 0 のとき
      -- otherwise は、 True と等価
      | otherwise -> v -- Just v にマッチし、v < 0 でないとき

guardPattern :: IO ()
guardPattern = do
  print (absMaybeFn (Just (123 :: Int))) -- output: 123
  print (absMaybeFn (Just (-123 :: Int))) -- output: 123
  print (absMaybeFn (Nothing :: Maybe Int)) -- output: 0

-- if式
{-
if 条件式 then 式1 else 式2

if"式"は式なので、値を持つ。
そのため、elseが省略できない上に、式1はおろか式2も省略できないことに注意

if式は、以下のcase式の糖衣構文に過ぎない。
case 式 of
  True -> 式1
  False -> 式2
-}

ifPattern :: IO ()
ifPattern = do
  print (if True then (1 :: Int) else (2 :: Int)) -- output: 1
  print (if False then (1 :: Int) else (2 :: Int)) -- output: 2

-- let式
-- 変数や補助関数を局所的に定義する
{-
let 宣言1
    宣言2
    ...
    宣言n
  in 式

宣言iで定義した変数または関数は、変数i+1以降と式にのみスコープを持つ。
そして、式がこのlet式全体の値になる。
-}
letArea :: (Fractional a) => a -> a
letArea r =
  let pi = 3.14
      square x = x * x
   in pi * square r

-- where節 (letS式とは別にwhere節もある)
-- 変数や補助関数を局所的に定義する
-- ※以下、let式の例と同じ処理を書いている。
whereArea :: (Fractional a) => a -> a
whereArea r = pi * square r
  where
    pi = 3.14
    square x = x * x
