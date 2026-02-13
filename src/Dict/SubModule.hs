-- 階層化されたモジュール
{-
モジュール名は、ファイル名 (階層構造を含む) と一致しなければならない。
したがって、Dict/SubModule.hs というファイルに対応するモジュール名は
Dict.SubModule である。
-}

module Dict.SubModule (subModuleEntity) where

subModuleEntity :: IO ()
subModuleEntity = putStrLn "Hello, from SubModule!"
