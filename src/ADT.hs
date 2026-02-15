module ADT
  ( adt,
    printArea,
    printHoliday,
    printTree,
    printPersonTaro,
    printPersonTaroJiro,
    printSynonym,
    printDigitString,
    printRatio,
    printDeclaration,
  )
where

-- 代数的データ型 (Algebraic Data Type, ADT)
-- 項の形で表されるデータ型の集合

adt :: IO ()
adt = putStrLn "-- ADT --"

-- データ型の定義
{-
data 型構成子 = データ構成子 ...

{- 直積型 -}
直積型とは、複数の値を同時に持つ型のことである。
data 型構成子 = データ構成子1 データ構成子2 ... データ構成子n

{- 直和型 -}
直和型とは、複数の型の選択肢のうち、いずれか一つを持つ型のことである。
data 型構成子 = データ構成子1 | データ構成子2 | ... | データ構成子n

代数的データ型の代数の対応関係は、以下のように考えれば良さそう
数値 (1,2,3,...) → 型が取りうる値の個数 (Boolであれば、True か False の2択)
変数 (a,b,c) → 型そのもの (T, U, V)
演算 (+, *) → 型の構成方法

直積型の具体例
-- Boolには、True or False の2通りの値がある
-- 2 * 2 = 4
data Point
  = X Bool
  | Y Bool

直和型の具体例
-- 2 + 2 = 4
enum Either {
  A(Bool),
  B(Bool)
}

-}

-- ex) 図形の型を定義する (直積型)
data Shape
  = Rect Double Double -- 矩形 (幅, 高さ)
  | Triangle Double Double -- 三角形 (底辺, 高さ)
  | Circle Double -- 円 (半径)

-- 面積を求める関数
area :: Shape -> Double
area (Rect x y) = x * y
area (Triangle x y) = 0.5 * x * y
area (Circle r) = pi * r * r

printArea :: IO ()
printArea = do
  let rect = (Rect 10 20 :: Shape)
      triangle = (Triangle 10 20 :: Shape)
      circle = (Circle 10 :: Shape)
  putStrLn $ "Rectangle area: " ++ show (area rect)
  putStrLn $ "Triangle area: " ++ show (area triangle)
  putStrLn $ "Circle area: " ++ show (area circle)

-- ex) 曜日の型を定義する (直和型)
data Weekly = Mon | Tue | Wed | Thu | Fri | Sat | Sun

holiday :: Weekly -> Bool
holiday Sat = True
holiday Sun = True
holiday _ = False

printHoliday :: IO ()
printHoliday = do
  print $ "holiday Mon: " ++ show (holiday Mon) -- output: False
  print $ "holiday Tue: " ++ show (holiday Tue) -- output: False
  print $ "holiday Wed: " ++ show (holiday Wed) -- output: False
  print $ "holiday Thu: " ++ show (holiday Thu) -- output: False
  print $ "holiday Fri: " ++ show (holiday Fri) -- output: False
  print $ "holiday Sat: " ++ show (holiday Sat) -- output: True
  print $ "holiday Sun: " ++ show (holiday Sun) -- output: True

-- パラメトリック多相型を実現できる
-- ex) Tree型を定義する
-- 型構成子をデータ構成子としても扱うことで、
-- 再帰的なデータ型を定義できる。 (再帰的デー型)
data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show)

printTree :: IO ()
printTree = do
  let tree1 = (Empty :: Tree Int)
      tree2 = (Leaf 10 :: Tree Int)
      tree3 = (Node (Node (Leaf 50) 60 Empty) 70 (Leaf 80) :: Tree Int)
  print tree1
  print tree2
  print tree3

-- フィールドラベル (Rustの構造体のようなもの)
-- ex) Person型を定義する
data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Show)

-- 宣言
-- name, ageがフィールドラベルになる
taro :: Person
taro = Person {name = "Taro", age = 30}

printPersonTaro :: IO ()
printPersonTaro = do
  print $ taro
  -- フィールドへのアクセス
  print $ "name: " ++ (name taro)
  print $ "age: " ++ show (age taro)

-- フィールドの更新 (新しい値を持つ新しいPersonを生成)
-- immutableなので、参照をしない限り必ず新しい変数に束縛する必要がある。
jiro :: Person
jiro = taro {age = (age taro) - 1}

dec :: Person -> Person
dec person = person {age = (age person) - 1}

printPersonTaroJiro :: IO ()
printPersonTaroJiro = do
  print $ taro
  print $ jiro
  -- 以下のように簡単にフィールドを更新 (新しいPersonを生成) できる。
  print $ age (dec taro)

-- 型シノニム (型表現のエイリアス)
-- 元の型と区別されない
-- ex) IntList型を定義する
type IntList = [Int]

{-
Rustで記述した場合の以下とおおよそ同じである。
type IntList = Vec<i32>;
-}

-- 以下のように使うことができる
sumIntList :: IntList -> Int
sumIntList xs = sum xs

-- ex) 型変数が含まれる場合
type Pair a b = (a, b)

-- 以下のように使うことができる
showPair :: (Show a, Show b) => Pair a b -> String
showPair (x, y) = "(" ++ show x ++ ", " ++ show y ++ ")"

printSynonym :: IO ()
printSynonym = do
  let intList = [1, 2, 3, 4, 5] :: IntList
      pair1 = (10, "Hello") :: Pair Int String
      pair2 = (3.14, True) :: Pair Double Bool
  print $ "Sum of IntList: " ++ show (sumIntList intList)
  print $ "Pair1: " ++ showPair pair1
  print $ "Pair2: " ++ showPair pair2

-- newtype 宣言
{-
データ構成子数 1、フィールド数 1 のみ許される data宣言
newtype 宣言で定義された型は、元の型と区別されるが、
元の型と同じ内部表現で扱われる。
-}
-- ex) Age型を定義する
newtype DigitString = DigitStr String
  deriving (Show)

{-
Rustで記述した場合の以下とおおよそ同じである。
struct DigitString(String);
-}

atoi :: DigitString -> Int
atoi (DigitStr xs) = read xs :: Int

printDigitString :: IO ()
printDigitString = do
  let ds = DigitStr "12345"
  print $ "Int: " ++ show (atoi ds)

-- 中置データ構成子
-- データ構成子には、 : で始まる演算子を使うことができる
-- ex) 有理数型を定義する
data Ratio = Integer :/ Integer deriving (Show)

ratioToFloat :: Ratio -> Float
ratioToFloat (x :/ y) = fromInteger x / fromInteger y

printRatio :: IO ()
printRatio = do
  print $ ratioToFloat (3 :/ 4) -- output: 0.75

{-
{-data宣言 と newtype宣言の稚違い-}
{-data宣言-}
ランタイムでもラッパーが存在し、ポインタ追跡が必要らしい
柔軟な代わりに、ゼロコスト抽象化ができないので、オーバーヘッドがある。

{-newtype宣言-}
コンパイル時のみ存在し、ランタイムでは中身の型と同一の表現になる。
つまり、ゼロコスト抽象化ガされているので、オーバーヘッドがない。
-}

{-newtype宣言 と 型シノニム(type)の違い-}
{-
{-newtype宣言-}

newtype宣言では、コンパイラが元の型と異なる型として解釈する。
従って、実質 Int 同士の足し算であっても、
myNewTypeIntIntAddr a  b = a + b
という風に掛くことはできず、以下のように書かなければならない。
-}
newtype MyNewTypeInt = MyNewTypeInt Int

myNewTypeIntIntAddr :: Int -> MyNewTypeInt -> Int
myNewTypeIntIntAddr a (MyNewTypeInt b) = a + b

{-
{-型シノニム-}

型シノニムは、コンパイラが元の型と全く同じ型として解釈する。
従って、以下のような記述が可能である。
-}
type MyInt = Int

myIntIntAddr :: Int -> MyInt -> Int
myIntIntAddr a b = a + b

printDeclaration :: IO ()
printDeclaration = do
  print $ myNewTypeIntIntAddr 1 (MyNewTypeInt 1)
  print $ myIntIntAddr 1 1

{-
{- 参考 -}
実際の有理数型と複素数型は以下のように定義されている。
data Ratio a = a :% a
data Complex a = a :+ a
-}
