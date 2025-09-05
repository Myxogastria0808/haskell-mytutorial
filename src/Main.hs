module Main (main) where

import Base (base, helloWorld, layoutPattern, patternPattern, printAdd, variable)
import DataType (dataType)
import Formula (casePattern, formula, guardPattern, ifPattern)

{-
参考サイト
http://walk.northcol.org/haskell/
-}

main :: IO ()
main = do
  -- Base
  base
  helloWorld
  layoutPattern
  variable
  patternPattern
  printAdd
  -- Formula
  formula
  casePattern
  guardPattern
  ifPattern
  -- DataType
  dataType
