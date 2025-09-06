module Main (main) where

import Base
  ( base,
    helloWorld,
    layoutPattern,
  )
import DataType (dataType)
import Formula
  ( casePattern,
    formula,
    guardPattern,
    ifPattern,
  )
import Function
  ( function,
    printAdd,
    printDouble,
    printEvenOdd,
    printFactorial,
    printFibonacci,
    printFunctionComposition,
    printLambdaAdd,
    printMap,
    printPower,
    printPowerWithGuard,
    printTwice,
  )
import List
  ( defineList,
    list,
    printOriginalMap,
  )
import Operator
  ( operator,
    printOriginalOperator,
  )
import Variable
  ( patternPattern,
    variable,
    variablePattern,
  )

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
  -- Variable
  patternPattern
  variable
  variablePattern
  -- DataType
  dataType
  -- Formula
  casePattern
  formula
  guardPattern
  ifPattern
  -- Function
  function
  printAdd
  printFibonacci
  printEvenOdd
  printFactorial
  printPower
  printPowerWithGuard
  printLambdaAdd
  printDouble
  printMap
  printTwice
  printFunctionComposition
  -- Operator
  operator
  printOriginalOperator
  -- List
  list
  defineList
  printOriginalMap
