{-
module Main (main) where が省略されたファイルを
Mainモジュールとするので、Mainモジュールでは、
module Main (main) whereと書かなくても良い
-}
module Main (main) where

import ADT
  ( adt,
    printArea,
    printDigitString,
    printHoliday,
    printPersonTaro,
    printPersonTaroJiro,
    printRatio,
    printSynonym,
    printTree,
  )
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
import LazyEvaluation (lazyEvaluation)
import List
  ( defineList,
    list,
    printListFold,
    printNumberSequence,
    printOriginalMap,
    printQsort,
  )
import Module
  ( Area (CircleArea),
    Shape (..),
    moduleFunc,
    printSubModule,
  )
import Operator
  ( operator,
    printOriginalOperator,
  )
import Planet (planet, printPlanet)
import TypeClass
  ( printAddSample,
    printHuman,
    printPoint1D,
    printPoint2D,
    printPoint3D,
    typeClass,
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
  printListFold
  printNumberSequence
  printQsort
  -- ADT
  adt
  printArea
  printHoliday
  printTree
  printPersonTaro
  printPersonTaroJiro
  printSynonym
  printDigitString
  printRatio
  -- TypeClass
  typeClass
  printAddSample
  printPoint2D
  printPoint1D
  printPoint3D
  printHuman
  -- Planet
  planet
  printPlanet
  -- Module
  moduleFunc
  printSubModule
  print $ CircleArea 10
  -- RectangleAreaはエクスポートされていないのでエラーになる
  -- print $ RectangleArea 10 20
  print $ Circle 10
  print $ Rectangle 10 20
  -- Lazy Evaluation
  lazyEvaluation
