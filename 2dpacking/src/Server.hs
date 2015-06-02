{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, RecordWildCards #-}

import GHC.Generics
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Function
import Debug.Trace

import Web.Scotty

import Control.Concurrent.ParallelIO.Global ( parallel )

import Data.Aeson ( FromJSON(..), ToJSON(..), (.:), Value(..) )

import qualified Model as Model
import Language.Minion


data Shapes = Shapes { pieces :: [[[Bool]]]
                     , pieceColours :: [String]
                     , packingDim :: (Int, Int)
                     , isRandom :: Bool
                     }
    deriving (Eq, Ord, Show, Generic)

instance FromJSON Shapes
instance ToJSON Shapes

data Packing = Packing { packing :: [[Int]]
                       , owners  :: [[Int]]
                       }
    deriving (Eq, Ord, Show, Generic)

data Packings = Packings { bitsPerId :: [ [[Bool]] ]
                         , colourPerId :: [ String ]
                         , packings :: [Packing]
                         }
    deriving (Eq, Ord, Show, Generic)


data PackingsJSON = PackingsJSON { newPacking :: [[PackingJSON]] }
    deriving (Eq, Ord, Show, Generic)
instance FromJSON PackingsJSON
instance ToJSON PackingsJSON


data PackingJSON = PackingJSON { x :: Int, y :: Int, shape :: [[Bool]], colour :: String }
    deriving (Eq, Ord, Show, Generic)
instance FromJSON PackingJSON
instance ToJSON PackingJSON

toNew :: Packings -> PackingsJSON
toNew Packings{..} = PackingsJSON
    [ [ PackingJSON{..}
      | (x,row    ) <- zip [0..] (packing p)
      , (y,shapeId) <- zip [0..] row
      , let shape  = bitsPerId !! (shapeId-1)
      , let colour = colourPerId !! (shapeId-1)
      , shapeId /= 0
      ]
    | p <- packings
    ]


main :: IO ()
main = scotty 3000 $ do
    post "/solve" $ do
        shapes@Shapes{..} <- jsonData
        -- liftIO $ print shapes
        let param =
                Model.Params
                    { Model.pieces =
                        [ Model.Piece {..}
                        | (bits, kind) <- zip pieces pieceColours
                        , let orientation = Model.Given
                        , let pieceID = 0
                        , any (==True) (concat bits)            -- if bits is all false, discard
                        ]
                    , Model.pieceDim = 8
                    , Model.kindRatio = [ (col, 1) | col <- pieceColours ]
                    , Model.packingDim = packingDim
                    }

        -- liftIO $ print param
        let paramPrepped = Model.prepPieces param
        let bitsPerId = map snd $ sortBy (compare `on` fst)
                            [ (pieceID, bits)
                            | Model.Piece{..} <- Model.pieces paramPrepped
                            ]
        let colourPerId = map snd $ sortBy (compare `on` fst)
                            [ (pieceID, kind)
                            | Model.Piece{..} <- Model.pieces paramPrepped
                            ]
        m <- runMinionBuilder (Model.model paramPrepped)
        -- liftIO $ print m
        let timePerMinion = 60
        let nbRuns = 4
        let iRuns = if isRandom then [1..nbRuns] else [0]
        solss <- liftIO $ parallel $ flip map iRuns $ \ i -> do
                    let opts = if i == 0 then [CpuLimit timePerMinion]
                                         else [CpuLimit timePerMinion, RandomiseOrder]
                    putStrLn $ "Running Minion " ++ show i
                    sols <- runMinion (show i) opts m
                        -- [RandomiseOrder, FindAllSols, SolLimit 20]
                        -- [RandomiseOrder, CpuLimit 10]
                        -- [CpuLimit 60]
                        (\ xs -> do putStrLn $ show i ++ " -- " ++ show (map snd xs)
                                    return xs )
                    if null sols
                        then putStrLn $ "(" ++ show i ++ ") Finished with no solutions."
                        else putStrLn $ "(" ++ show i ++ ") Finished with scrap: " ++ show (head (last sols))
                    return sols

        -- just all runs
        -- let sols = concat solss

        -- merging and sorting all runs, jumps back and forth too much in the animation
        -- let sols = reverse
        --                 $ nubBy  ((==)    `on` (snd . head))
        --                 $ sortBy (compare `on` (snd . head))
        --                 $ concat solss

        -- picking the best run
        let sols = (\ xs -> if null xs then [] else snd $ head xs )     -- pick the smallest lastScrap, only keep the run
                 $ sortBy (compare `on` fst)
                    [ (lastScrap, run)
                    | run <- solss
                    , not (null run)
                    , let lastScrap = snd $ head $ last run
                    ]

        if null sols
            then do
                liftIO $ putStrLn "Sending all 0s"
                json $ toNew $ Packings bitsPerId colourPerId
                      [Packing [ [ 0
                                 | i <- [1..snd packingDim]
                                 ]
                               | j <- [1..fst packingDim]
                               ]
                               [ [ 0
                                 | i <- [1..snd packingDim]
                                 ]
                               | j <- [1..fst packingDim]
                               ]
                      ]
            else do
                liftIO $ putStrLn $ "Sending solutions: " ++ show (length sols)
                liftIO $ putStrLn $ "With scrap       : " ++ show (snd $ head $ last sols)
                -- let finalSol = chunk (snd packingDim) $ map snd (last sols)
                -- liftIO $ putStrLn "Final solution"
                -- liftIO $ mapM_ print finalSol
                json $ toNew $ Packings bitsPerId colourPerId
                    [ Packing finalSol owners
                    | sol <- sols
                    , let finalSol = chunk (snd packingDim) $ take (fst packingDim * snd packingDim) $ drop 1 $ map snd sol
                    , let owners   = chunk (snd packingDim) $ drop (fst packingDim * snd packingDim) $ drop 1 $ map snd sol
                    ]

    get "/:filename" $ do
        filename <- param "filename"
        liftIO $ putStrLn $ "serving " ++ filename
        file filename

chunk :: Int -> [a] -> [[a]]
chunk n xs | length xs <= n = [xs]
chunk n xs = take n xs : chunk n (drop n xs)
