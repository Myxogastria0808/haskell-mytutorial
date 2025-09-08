module TypeClass
  ( typeClass,
    printAddSample,
    printPoint2D,
    printPoint1D,
    printPoint3D,
    printHuman,
  )
where

typeClass :: IO ()
typeClass = putStrLn "-- TypeClass --"

{-
https://qiita.com/mrsekut/items/779034c7df1e1a2a309e
-}

-- 型クラス (type class)
{-
型クラスは、データ型をカテゴライズする役割を持つ概念。
例えば、数値型全般を表す Num 型クラスは、数値型全般を表す。
Num型のクラスのインスタンスは、具体的な数値型である Int や Float などがある。

Numという型クラスのインスタンスとして、IntやFloatなどの具体的な型があるイメージ。
  +----- Num ------+
  | Int, Integer   |
  | Float, Double  |
  +----------------+

{-OOPのクラスとの比較-}
OOPのクラスは、データ型であり、オブジェクト。
Haskellの型クラスは、データ型をカテゴライズするための概念。
そして、型クラスのインスタンスは、具体的なデータ型。
なので、型クラスは、OOPのクラスの一つ上の概念である。
-}

-- 型クラスの利用
-- 関数の型定義のときに度々登場していたもの
{-
-- 以下のように、型クラスを指定することができる
f :: (C a, D a, E b) => a -> b -> a
-}
-- 例えば、以下のような関数
-- これは、Num型クラスのインスタンスである型に対して使える関数
addSample :: (Num a) => a -> a -> a
addSample a b = a + b

printAddSample :: IO ()
printAddSample = do
  print (addSample (3 :: Int) (5 :: Int)) -- Int型
  print (addSample (3 :: Integer) (5 :: Integer)) -- Integer型
  print (addSample (3.5 :: Float) (5.2 :: Float)) -- Float型
  print (addSample (3.5 :: Double) (5.2 :: Double)) -- Double型

-- 型クラスの定義
{-
 * ghciのRepl環境で、`:i 型クラス名`を実行すると、型クラスの定義を見ることができる。
例えば、Eq は以下のように定義されている。
各型クラスは、{- # MINIMAL # -} で指定されているクラスメソッドを最低限実装する必要がある。
Eq の場合は、(==) または (/=) のどちらかを実装すればよい。

ghci> :i Eq
type Eq :: * -> Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}

-- Defined in ‘GHC.Classes’
instance Eq Integer -- Defined in ‘GHC.Num.Integer’

instance (Eq a, Eq b) => Eq (Either a b)

-- Defined in ‘GHC.Internal.Data.Either’
instance ([safe] Eq a) => Eq (Maybe a)

(以下、略)

{- Eq クラスの具体的な定義 -}
class宣言には、クラスメソッドの型シグネチャ宣言とデフォルト定義を記述する。
なお、デフォルト定義は省略可能

class Eq a where
    -- クラスメソッドの型シグネチャ宣言
    (==), (/=) :: a -> a -> Bool
    -- デフォルト定義 (省略可能)
    x == y = not (x /= y)
    x /= y = not (x == y)
-}

-- インスタンス宣言
-- 型クラスの型シグネチャ宣言の具体的な実装を行う。
-- ex) 2次元空間上の点を表す Point型を考える。
data Point2D = Position2D Double Double

-- Eqクラスのインスタンス宣言
instance Eq Point2D where
  (Position2D x y) == (Position2D x' y') = (x == x') && (y == y')

printPoint2D :: IO ()
printPoint2D = do
  print $ (Position2D 1.0 1.0) == (Position2D 1.0 1.0) -- True
  print $ (Position2D 1.0 1.0) == (Position2D 1.0 2.0) -- False
  print $ (Position2D 2.0 1.0) == (Position2D 1.0 1.0) -- False
  print $ (Position2D 1.0 1.0) == (Position2D 2.0 2.0) -- False

-- クラスの継承
{-
すでにある型クラスを継承して、新しい型クラスを定義できる。
    A
   / \
  B   C
   \ /
    D
多重継承が可能なので、上記のような継承もできる。
単に、DはA, B, Cの型クラスを継承しているという関係になる。
型クラスDのインスタンスを実装する対象は、
A, B, Cのメソッドも実装する必要がある。
(Bで再定義したAのメソッドの定義などが存在しないので、OOPの多重継承の問題は発生しない)

{-単一のクラスを継承する場合-}
class C a => D a where
    ...
{-複数のクラスを継承する場合-}
class (C a, D a) => E a where
    ...

{- Ord クラスの具体的な定義 -}
Ordクラスは、Eqクラスを継承している。
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min :: a -> a -> a
    --(デフォルト実装は省略)

data Ordering = LT | EQ | GT
-}
-- ex) 1次元空間上の点を表す Point1D型を考える。
data Point1D = Position1D Double

-- それぞれの型クラスのMINIMALを満たすように実装する。
-- Eqクラスのインスタンス宣言
instance Eq Point1D where
  (Position1D x) == (Position1D x') = x == x'

-- Ordクラスのインスタンス宣言
instance Ord Point1D where
  compare (Position1D x) (Position1D x')
    | x < x' = LT
    | x == x' = EQ
    | otherwise = GT

printPoint1D :: IO ()
printPoint1D = do
  print $ (Position1D 1.0) == (Position1D 1.0) -- True
  print $ (Position1D 1.0) == (Position1D 2.0) -- False
  print $ (Position1D 2.0) == (Position1D 1.0) -- False
  print $ (Position1D 2.0) > (Position1D 1.0) -- True
  print $ (Position1D 1.0) < (Position1D 2.0) -- True
  print $ (Position1D 1.0) >= (Position1D 1.0) -- True
  print $ (Position1D 1.0) <= (Position1D 1.0) -- True

-- deriving宣言
-- クラスメソッドの実装が自明な場合、
-- deriving宣言を用いると、自動的にインスタンス宣言を生成してくれる。
-- (instance宣言を省略できる)
data Point3D = Position3D Double Double Double deriving (Eq)

printPoint3D :: IO ()
printPoint3D = do
  print $ (Position3D 1.0 2.0 3.0) == (Position3D 1.0 2.0 3.0) -- True
  print $ (Position3D 1.0 2.0 3.0) == (Position3D 1.0 2.0 4.0) -- False

-- 型クラスのサンプル
{-
      Creature
         *
       /   \
      /     \
  Thinking  Sex
     *       *
      \     /
       \   /
        \ /
       Speaking
-}

data LifeStatus = Life | Death deriving (Show)

data SexType = Male | Female | Other deriving (Show)

-- Creature class
class Creature a where
  lifeStatus :: a -> LifeStatus

-- Sex class
class (Creature a) => Sex a where
  sexType :: a -> SexType

-- Thinking class
class (Creature a) => Thinking a where
  isThink :: a -> Bool

-- Speaking class
class (Sex a, Thinking a) => Speaking a where
  speakLanguage :: a -> String

-- Human data type
data Human = Profile
  { status :: LifeStatus,
    sex :: SexType,
    think :: Bool,
    country :: String,
    language :: String
  }

-- instance declarations
instance Creature Human where
  lifeStatus Profile {status = s} = s

instance Sex Human where
  sexType Profile {sex = s} = s

instance Thinking Human where
  isThink Profile {think = t} = t

instance Speaking Human where
  speakLanguage Profile {language = l} = l

taro :: Human
taro =
  Profile
    { status = Life,
      sex = Male,
      think = True,
      country = "Japan",
      language = "Japanese"
    }

hanako :: Human
hanako =
  Profile
    { status = Life,
      sex = Female,
      think = True,
      country = "Japan",
      language = "Japanese"
    }

john :: Human
john =
  Profile
    { status = Life,
      sex = Male,
      think = True,
      country = "USA",
      language = "English"
    }

printHuman :: IO ()
printHuman = do
  print $ lifeStatus taro
  print $ sexType taro
  print $ isThink taro
  print $ speakLanguage taro
  print $ lifeStatus hanako
  print $ sexType hanako
  print $ isThink hanako
  print $ speakLanguage hanako
  print $ lifeStatus john
  print $ sexType john
  print $ isThink john
  print $ speakLanguage john
