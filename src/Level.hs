module Level where

import Control.Applicative
import Control.Monad
import Data.Fixed
import Data.Maybe
import FRPEngine.Types
import Linear
import Types

boxColl :: (Number a) => [[V2 a]]
boxColl = [[(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)]]

moveBlock mod a = a + (99 * mod)

moveBlockUp = moveBlock 1

moveBlockDown = moveBlock (-1)

pureRandom :: (Real a) => a -> V2 a
pureRandom a = (V2 (3000 + a * 200) ((a ^ 5) `mod'` 501))

type Slice = [Bool]

type Lvl = [Slice]

build :: (Number a) => Lvl -> [CollObj a SpriteSelect]
build lvl = join $ zipWith (flip buildSlice) lvl y
  where
    -- 99 is here to fix the gap problem with integer rendering positions
    y = realToFrac <$> [0,99 ..]

buildSlice :: (Number a) => a -> Slice -> [CollObj a SpriteSelect]
buildSlice x things@(a : b : c : d) =
  catMaybes $ buildSingle <$> (zip pos things)
  where
    y = realToFrac <$> [-1,98 .. 506]
    pos = liftA2 V2 [x] y

buildSingle :: (Number a) => (V2 a, Bool) -> Maybe (CollObj a SpriteSelect)
buildSingle ((V2 x y), True) =
  Just
    ( CollObj
        boxColl
        (Obj (V2 x y) 0 0 (V2 100 100) SobjectSprite True)
    )
buildSingle ((V2 x y), False) =
  Nothing

initialGame :: GameState
initialGame =
  GameState
    (CameraState 3)
    ( PhysicalState
        ( CollObj
            boxColl
            (Obj (V2 500 200) 0 0 (V2 100 100) SobjectSprite2 True)
        )
        ( [ -- Terrain
            ( CollObj
                boxColl
                (Obj (V2 (-10000) 542) 0 0 (V2 50000000 5000) SobjectSprite False)
            ),
            ( CollObj
                boxColl
                (Obj (V2 (-10000) (-5050)) 0 0 (V2 50000000 5000) SobjectSprite False)
            )
          ]
            ++ (build
               [
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [True, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, True, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [True, True, True, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, True, True, True],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [True, True, True, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, True, True, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, True, False, False],
                 [False, False, False, False, False, False],
                 [False, True, False, True, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, True, True, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [True, False, False, True, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, True, True, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, True, False],
                 [False, True, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, True, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, True, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, True, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [True, False, False, True, False, False],
                 [False, False, False, False, False, False],
                 [False, False, True, False, False, False],
                 [False, False, False, True, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, True, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, False, False, False],
                 [False, False, False, True, False, False],
                 [False, False, False, False, False, False]
               ]
              )
          -- Semi-random level
          -- ++ ((\a -> CollObj boxColl (Obj (pureRandom a) (V2 100 100) 0 SobjectSprite True)) <$> [2 .. 13] ++ [15 .. 31] ++ [34 .. 49] ++ [51 .. 56] ++ [58 .. 91])
        )
    )
    True
