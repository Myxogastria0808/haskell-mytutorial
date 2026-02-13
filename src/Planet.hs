module Planet (planet, printPlanet) where

{-
これは、Haskellの型クラスを理解するための例となっている。
以下の物語を型クラスで表現したものとなっている。

嗅覚の情報のみで等価性を判断をする生命体がいたとして、
その生命体の個体 lifeA と個体 lifeB が同じ匂い (匂いにおいて 、 A == B がTrue) だとした場合、
たとえ、人間から見たら個体 lifeA と個体 lifeB の見た目が全然異なっていたとしても、
その生命体にとっての定義 (型クラス Eq の SmellMonster についての instance) においては等しいものだと扱うものだとする。
このような状況であっても、問題なくEqという型クラスのインスタンス化は可能である。
(SmellMonsterのEqのinstanceでは、smellのみで等価性を判断することで上記の状況を表現している)
-}

data Smell = A | B | C | D deriving (Show)

data Looks = A' | B' | C' | D' deriving (Show)

data SmellMonster = SmellMonster {smell :: Smell, looks :: Looks} deriving (Show)

instance Eq Smell where
  -- 等価の定義を明示的に行っている
  A == A = True
  B == B = True
  C == C = True
  D == D = True
  _ == _ = False

-- 型クラス Eq そのものの定義に関しては、(==) :: a -> a -> Bool という関数が定義されている。
-- したがって、型クラスの段階では、結果としてBoolを返せさえすれば良く、
-- どのように等価性を判断するかはインスタンス化の際に決めれば良い。
instance Eq SmellMonster where
  -- smellのみで等価性を判断
  (SmellMonster s1 _) == (SmellMonster s2 _) = (s1 == s2)

lifeA :: SmellMonster
lifeA = SmellMonster {smell = A, looks = A'}

lifeB :: SmellMonster
lifeB = SmellMonster {smell = A, looks = B'}

planet :: IO ()
planet = putStrLn "-- Planet --"

printPlanet :: IO ()
printPlanet = do
  print lifeA
  print lifeB
  print $ "lifeA == lifeB: " ++ show (lifeA == lifeB) -- smellが同じなのでTrue
