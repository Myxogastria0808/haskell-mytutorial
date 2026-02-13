{-
{-モジュールのエクスポート-}
module M where ... 全てのエンティティをエクスポート
module M (x) where ... エンティティxのみをエクスポート
module M (x, y) where ... エンティティxとyをエクスポート
module M (A) where ... 型構成子Aをエクスポート
module M (A(..)) where ... 型構成子Aとその全てのデータ構成子をエクスポート
module M (A(B, C)) where ... 型構成子Aとそのデータ構成子BとCをエクスポート

以下のエクスポートの例では、型構成子Shapeは全てのデータ構成子をエクスポートし、
型構成子Areaはデータ構成子CircleAreaのみをエクスポートする
-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Module (moduleFunc, printSubModule, Shape (..), Area (CircleArea)) where

{-
{-モジュールのインポート-}
import M .. 全てのエンティティをインポート
import M (x) ... エンティティxのみをインポート
import M hiding (x) ... エンティティx以外をインポート
import qualified M ... モジュール名による修飾を必須にする (M.x のようにしないといけない)
import M as N ... モジュール名MをNと置き替える

※標準モジュールの一つであるPreludeは、
  暗黙的に全てのエンティティをインポートしている。
  なので、hidingは、Preludeに対して使うことが多い。 (個人の見解)
-}
import qualified Dict.SubModule

moduleFunc :: IO ()
moduleFunc = putStrLn "-- Module --"

-- SubModuleの関数を呼び出す
printSubModule :: IO ()
-- import qualified なので、subModuleではエラーになる
printSubModule = Dict.SubModule.subModuleEntity

data Shape
  = Circle Double
  | Rectangle Double Double
  deriving (Show)

data Area
  = CircleArea Double
  | RectangleArea Double Double
  deriving (Show)
