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
import List
  ( defineList,
    list,
    printListFold,
    printNumberSequence,
    printOriginalMap,
    printQsort,
  )
import Operator
  ( operator,
    printOriginalOperator,
  )
import Planet (planet, printPlanet)
import TypeClass
  ( typeClass,
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
  -- Planet
  planet
  printPlanet
