module Main where

import Data.List
import Data.Maybe
import Data.Function
import Safe ( atMay )
import Language.Minion

import Model


main :: IO ()
main = do
    m <- runMinionBuilder (model (prepPieces params0))
    runMinion_
        [RandomiseOrder, CpuLimit 600]
        m
        print


params0 =
    Params { pieces = [ Piece { bits = [ [True, True, True]
                                       , [False, False, False]
                                       , [False, False, False]
                                       ]
                              , kind = "Red"
                              , orientation = Given
                              , pieceID = 0
                              }
                      , Piece { bits = [ [True, True, False]
                                       , [True, True, False]
                                       , [False, False, False]
                                       ]
                              , kind = "Blue"
                              , orientation = Given
                              , pieceID = 0
                              }
                      ]
           , pieceDim = 3
           , kindRatio = [ ("Red", 1)
                         , ("Blue", 2)
                         ]
           , packingDim = (20, 20)
           }
