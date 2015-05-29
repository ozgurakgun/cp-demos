{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Model where

import Data.List
import Data.Maybe
import Data.Function
import Safe ( atMay )
import Language.Minion


data Orientation = Given | Rot90 | Rot180 | Rot270
    deriving (Show)

type PieceKind = String

data Piece = Piece
    { bits :: [[Bool]]                  -- a square, where True means full and False means empty
    , kind :: PieceKind                 -- colour
    , orientation :: Orientation
    , pieceID :: Int                    -- a unique id for a piece identified by (kind, orientation)
    }
    deriving (Show)

rotate :: [[a]] -> [[a]]
rotate = transpose . reverse

shiftUp :: [[Bool]] -> [[Bool]]
shiftUp xs = if all (False==) (head xs)
                then shiftUp (tail xs ++ [head xs])
                else xs

shiftLeft :: [[Bool]] -> [[Bool]]
shiftLeft xs = if all (False==) (map head xs)
                then shiftLeft [ tail row ++ [False] | row <- xs ]
                else xs

orientations :: Piece -> [Piece]
orientations p = nubBy ((==) `on` bits)
    [ p { bits        = shiftUp $ shiftLeft $                            bits p
        , orientation = Given
        }
    , p { bits        = shiftUp $ shiftLeft $                   rotate $ bits p
        , orientation = Rot90
        }
    , p { bits        = shiftUp $ shiftLeft $          rotate $ rotate $ bits p
        , orientation = Rot180
        }
    , p { bits        = shiftUp $ shiftLeft $ rotate $ rotate $ rotate $ bits p
        , orientation = Rot270
        }
    ]

data Params = Params
    { pieces :: [Piece]
    , pieceDim :: Int
    , kindRatio :: [(PieceKind, Int)]
    , packingDim :: (Int, Int)
    }
    deriving (Show)

prepPieces :: Params -> Params
prepPieces p = p { pieces = reID $ concatMap orientations $ pieces p }
    where reID :: [Piece] -> [Piece]
          reID ps = zipWith (\ piece i -> piece { pieceID = i } ) ps [1..]


model :: Monad m => Params -> MinionBuilder m ()
model Params{..} = do

    let nbPieces = length pieces
    let coordToInt (i,j) = (i-1) * snd packingDim + j

    (topLeft, topLeftVars) <- varVector (Discrete 0 nbPieces)           -- n=0 means this is not a top-left of a piece
        [ (i,j)                                                         -- n>0 means this is a top-left of piece n
        | i <- [1 .. fst packingDim]
        , j <- [1 .. snd packingDim]
        ]

    -- each cell has an owner, the corresponding topLeft coordinate
    -- or is 0
    (owner, ownerVars) <- varVector (Discrete 0 (fst packingDim * snd packingDim))
        [ (i,j)
        | i <- [1 .. fst packingDim]
        , j <- [1 .. snd packingDim]
        ]

    postConstraint
        [ Cwatched_or ( Cw_literal (topLeft (i,j)) 0
                      : topLeftOfAPiece
                      )
        | i <- [1 .. fst packingDim]
        , j <- [1 .. snd packingDim]
        , let topLeftOfAPiece =
                [ Cwatched_and ( (Cw_literal (topLeft (i,j)) (pieceID p))
                               : owners
                               )
                | p <- pieces
                , let owners' = [ if i+i2 <= fst packingDim && j+j2 <= snd packingDim
                                     then Just $ Cw_literal (owner (i+i2, j+j2)) (coordToInt (i,j))
                                     else Nothing
                                | i2 <- [0..pieceDim-1]
                                , j2 <- [0..pieceDim-1]
                                , bits p !! i2 !! j2
                                ]
                , let owners = catMaybes owners'
                , length owners == length owners'           -- otherwise this piece isn't a candidate
                ]
        ]

    -- counting number of times a PieceKind occurs
    (countKind , countKindVars ) <- varVector (Discrete 0 (fst packingDim * snd packingDim))
                                              (nub $ map kind pieces)
    (countPiece, countPieceVars) <- varVector (Discrete 0 (fst packingDim * snd packingDim))
                                              (      map pieceID pieces)
    totalPieces  <- varDiscrete' "totalPieces"  0 (fst packingDim * snd packingDim)
    minCountKind <- varDiscrete' "minCountKind" 0 (fst packingDim * snd packingDim)
    maxCountKind <- varDiscrete' "maxCountKind" 0 (fst packingDim * snd packingDim)
    scrap        <- varDiscrete' "scrap"        0 (fst packingDim * snd packingDim)
    postConstraint
        [ Coccurrence topLeftVars i (countPiece i)
        | i <- map pieceID pieces
        ]
    postConstraint
        [ cSumEq ofKind (countKind k)
        | k <- nub $ map kind pieces
        , let ofKind = [ countPiece (pieceID p)
                       | p <- pieces
                       , kind p == k
                       ]
        ]
    postConstraint $ cSumEq countPieceVars totalPieces
    postConstraint $ Cmin countKindVars minCountKind
    postConstraint $ Cmax countKindVars maxCountKind
    postConstraint $ cWeightedSumEq
        ( (1, scrap)
        : [ (bitCount, countKind (kind p))
          | p <- nubBy ((==) `on` kind) pieces
          , let bitCount = sum $ map (\ b -> if b then 1 else 0 ) (concat (bits p))
          ]
        )
        (constant (fst packingDim * snd packingDim))

    -- -- let's generate some tables
    -- postConstraint
    --     [ cleverTable
    --         [ topLeft (i1,j1)
    --         , topLeft (i2,j2)
    --         ]
    --         disalloweds
    --         allTuples
    --     | i1 <- [1 .. fst packingDim]
    --     , j1 <- [1 .. snd packingDim]
    --     , i2 <- [1 .. fst packingDim]
    --     , j2 <- [1 .. snd packingDim]
    --     , (i1, j1) /= (i2, j2)
    --     , let iDiff = i1 - i2
    --     , let jDiff = j1 - j2
    --     , let disalloweds =
    --             [ [pieceID p1, pieceID p2]
    --             | p1 <- pieces
    --             , p2 <- pieces
    --             , -- check for overlap here
    --               or  [ True
    --                   | a <- [0 .. pieceDim-1]
    --                   , b <- [0 .. pieceDim-1]
    --                   , let bitA = case atMay (bits p1) a of
    --                                   Nothing  -> False
    --                                   Just p1' -> case atMay p1' b of
    --                                       Nothing   -> False
    --                                       Just p1'' -> p1''
    --                   , let bitB = case atMay (bits p2) (a+iDiff) of
    --                                   Nothing  -> False
    --                                   Just p2' -> case atMay p2' (b+jDiff) of
    --                                       Nothing   -> False
    --                                       Just p2'' -> p2''
    --                   , bitA == True
    --                   , bitB == True
    --                   ]
    --             ]
    --     , let allTuples =
    --             [ [pieceID p1, pieceID p2]
    --             | p1 <- pieces
    --             , p2 <- pieces
    --             ]
    --     , not (null disalloweds)
    --     ]

    -- spread
    -- postConstraint =<< sequence
    --     [ do
    --         diff <- abs $ pure (countKind a) - pure (countKind b)
    --         return $ Cw_inset diff [0..4]
    --     | a <- nub $ map kind pieces
    --     , b <- nub $ map kind pieces
    --     , a < b
    --     ]


    -- postConstraint $ Csumleq [scrap] (constant 35)
    -- postConstraint $ Csumleq [minCountKind] (constant 20)

    -- symmetry breaking between pieces of the same kind

    -- obj <- pure minCountKind - pure scrap

    minimising scrap
    -- maximising minCountKind
    -- maximising obj
    searchOrder $ map (,Asc) (reverse topLeftVars)
    -- outputs $ [minCountKind, maxCountKind] ++ countKindVars ++ [scrap, totalPieces] -- ++ topLeftVars
    outputs (scrap:topLeftVars)

cleverTable vars@[v1,v2] disalloweds allTuples =
    if length disalloweds <= div (length allTuples) 2
        then Cnegativetable vars disalloweds
        else let alloweds = allTuples \\ disalloweds in
             Cwatched_or [ Ctable vars alloweds
                         , Cw_literal v1 0
                         , Cw_literal v2 0
                         ]


