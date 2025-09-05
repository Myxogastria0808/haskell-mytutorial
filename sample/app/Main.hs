module Main (main) where

main :: IO ()
main = do
  putStrLn "Hello, World!"
  io
  myBlock
  variable
  numberPattern
  charPattern
  stringPattern
  listPattern

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
  putStrLn "Block sample"
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
  putStrLn "Input sample"
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
  putStrLn "Variable sample"
  print (x + y)

-- 数値
numberPattern :: IO ()
numberPattern = do
  let decNum :: Int
      octNum :: Int
      hexNum :: Int
      doubleNum :: Double
      doubleNum2 :: Double
      decNum = 123 -- 10進数
      octNum = 0o123 -- 8進数
      hexNum = 0x123 -- 16進数
      doubleNum = 1.23 -- 単精度浮動小数点数
      doubleNum2 = 1.23e10 -- 単精度浮動小数点数 (指数表現)
   in do
        putStrLn "Number pattern sample"
        print decNum
        print octNum
        print hexNum
        print doubleNum
        print doubleNum2

-- 文字
charPattern :: IO ()
charPattern = do
  let char1 :: Char
      char2 :: Char
      char3 :: Char
      char1 = 'a' -- 半角英数字記号
      char2 = 'あ' -- Unicode文字
      char3 = '\x3042' -- Unicode文字 (あ)
   in do
        putStrLn "Character pattern sample"
        print char1
        print char2
        print char3

-- 文字列
stringPattern :: IO ()
stringPattern = do
  let str :: String
      -- 文字列を複数行に分割できる
      str =
        "Hello, \
        \World!"
   in do
        putStrLn "String pattern sample"
        print str

-- リスト
listPattern :: IO ()
listPattern = do
  putStrLn "List pattern sample"
  let lst1 :: [Int]
      lst2 :: [Int]
      lst3 :: [Int]
      lst4 :: [Int]
      lst5 :: [Char]
      lst6 :: [Char]
      elm1 :: Int
      lst7 :: [Int]
      len :: Int
      lst1 = [1, 2, 3] -- 基本のパターン
      lst2 = [1 .. 3] -- = [1, 2, 3] (範囲指定)
      lst3 = [3, 2 .. 1] -- = [3, 2, 1] (範囲指定)
      lst4 = [1, 3 .. 9] -- = [1, 3, 5, 7, 9] (範囲指定 + ステップ指定)
      lst5 = ['a', 'b', 'c'] -- 基本のパターン
      lst6 = ['a' .. 'c'] -- = ['a', 'b', 'c'] (範囲指定)
      elm1 = [1, 2, 3, 4] !! 2 -- リストの要素にアクセス (0始まり)
      lst7 = [1, 2, 3] ++ [4, 5, 6] -- リストの連結
      len = length [1, 2, 3] -- リストの長さ (=3)
   in do
        print lst1
        print lst2
        print lst3
        print lst4
        print lst5
        print lst6
        print elm1
        print lst7
        print len
