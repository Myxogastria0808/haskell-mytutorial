module Base
  ( base,
    helloWorld,
    layoutPattern,
  )
where

base :: IO ()
base = putStrLn "-- Base --"

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
