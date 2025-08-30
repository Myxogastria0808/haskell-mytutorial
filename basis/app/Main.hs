module Main (main) where

main :: IO ()
main = do
  putStrLn "Hello, World!"
  io
  myBlock
  variable

-- 一行コメントアウト
{-
  複数行コメントアウト
-}

-- ブロック
{-
複数の式を一つの式として扱います。doとブロックを組み合わせることで、
複数の式を実行することができる。
-}
myBlock :: IO ()
myBlock = do
  putStrLn "Red"
  putStrLn "Green"
  putStrLn "Blue"

{-
以下のように書くこともできる。
myBlock = do {
    putStrLn "Red"；
    putStrLn "Green"；
    putStrLn "Blue"；
  }
-}

-- 入出力
io :: IO ()
io = do
  -- Input
  putStrLn "Input sample: "
  putChar 'a' -- 文字の出力する
  putStr "ABC" -- 文字列を改行無しで出力する
  putStrLn "Hello, World!" -- 文字列を改行ありで出力する
  print "ABC" -- 任意の型の値を改行付きで出力する (デバッグ用)
  -- Output
  -- putStrLn "Output sample: "
  -- x <- getChar -- 文字の入力を受け取る
  -- putStrLn ("You entered: " ++ [x])
  -- y <- getLine -- 文字列の入力を受け取る
  -- putStrLn ("You entered: " ++ y)
  -- z <- getContents -- 標準入力からEOFまで読み込む
  -- putStrLn ("You entered: " ++ z)

-- 型
{-
Bool    -- True or False
Char    -- 文字型
String  -- 文字列型 (= [Char])
Int     -- 固定長整数 (最低 30 bits 以上)
Integer -- 多倍長整数 (any bits)
Float   -- 単精度浮動小数点数 (32 bits)
Double  -- 倍精度浮動小数点数 (64 bits)

[Int]       -- Intのリスト
[Char]      -- Charのリスト (= String)
(Int, Char) -- IntとCharのタプル

Int -> Int           -- Intを受け取り、Intを返す関数型
-- 以下がカリー関数
Int -> Int -> Double -- Intを受け取り、Intを受け取り、Doubleを返す関数型

a   -- 任意の型
[a] -- 任意の型のリスト
-}

-- 変数
{-
Haskellの変数は不変 (immutable) である。
そのため、代入ではなく束縛 (binding) と呼ばれる。
最初の文字は小文字でなければならない。
-}
variable :: IO ()
variable = do
  let x :: Int
      y :: Int
      x = 123
      y = 456
  print (x + y)