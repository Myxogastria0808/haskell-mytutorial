module Variable
  ( variable,
    variablePattern,
    patternPattern,
  )
where

variable :: IO ()
variable = putStrLn "-- Variable --"

-- 変数
{-
変数の型推論は自動で行われる。
変数に価を結び付けることを、変数の束縛 (binding) という。
※Haskellには、代入の概念はない (変数は不変)
-}
variablePattern :: IO ()
variablePattern = do
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
