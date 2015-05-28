{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, RecordWildCards #-}

import GHC.Generics
import Control.Monad.IO.Class
import Data.List
import Data.Function
import Debug.Trace

import Web.Scotty

import Data.Aeson ( FromJSON(..), ToJSON(..), (.:), Value(..) )

import qualified Model as Model
import Language.Minion


data Shapes = Shapes { pieces :: [[[Bool]]]
                     , pieceColours :: [String]
                     , packingDim :: (Int, Int)
                     }
    deriving (Eq, Ord, Show, Generic)

instance FromJSON Shapes
instance ToJSON Shapes

data Packing = Packing { packing :: [[Int]]
                       , bitsPerId :: [ [[Bool]] ]
                       , colourPerId :: [ String ]
                       }
    deriving (Eq, Ord, Show, Generic)

instance FromJSON Packing
instance ToJSON Packing

data Packings = Packings { packings :: [Packing] }
    deriving (Eq, Ord, Show, Generic)

instance FromJSON Packings
instance ToJSON Packings


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
        sols <- liftIO $ runMinion
            -- [RandomiseOrder, FindAllSols, SolLimit 20]
            [RandomiseOrder, CpuLimit 60]
            -- [CpuLimit 60]
            m
            (\ xs -> do print (map snd xs) ; return xs )
        if null sols
            then do
                liftIO $ putStrLn "Sending all 0s"
                json $ Packings
                      [Packing (chunk (snd packingDim) $ replicate (fst packingDim * snd packingDim) 0)
                               bitsPerId
                               colourPerId
                      ]
            else do
                liftIO $ putStrLn "Sending solutions"
                -- let finalSol = chunk (snd packingDim) $ map snd (last sols)
                -- liftIO $ putStrLn "Final solution"
                -- liftIO $ mapM_ print finalSol
                json $ Packings
                    [ Packing finalSol bitsPerId colourPerId
                    | sol <- sols
                    , let finalSol = chunk (snd packingDim) $ map snd sol
                    ]

    get "/:filename" $ do
        filename <- param "filename"
        liftIO $ putStrLn $ "serving " ++ filename
        file filename

chunk :: Int -> [a] -> [[a]]
chunk n xs | length xs <= n = [xs]
chunk n xs = take n xs : chunk n (drop n xs)
