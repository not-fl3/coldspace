{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Lens
import           Control.Monad
import qualified Graphics.GLUtil.Camera3D as U
import qualified Linear as L

import           Engine.Engine
import           Engine.Sygnals.Input
import qualified Engine.Sygnals.Keyboard as Keyboard
import qualified Engine.Sygnals.Mouse as Mouse
import           MyNodeData
import           SysLoader

import           Debug.Trace

type MSygnal = Sygnal MyNodeData

update :: (Float, (Maybe Key), Maybe (L.V3 Float)) -> Player -> Player
update (dt, _, Nothing) player = player
update (dt, _, (Just target)) player@Player{playerPosition = pos} | isfar target pos = player {playerPosition = pos  + delta}
                                                                  | otherwise = player
  where delta = (L.normalize (target - pos)) L.^/ 20
        isfar x y = (L.distance x y) > 2

cameraFollow :: (Player, (Int, Int), Bool) -> EulerCamera -> EulerCamera
cameraFollow (p@(Player {playerPosition = pos}), (dx, dy), rightPressed) c@(EulerCamera d  center (x, y))
  | rightPressed = c {cameraDistance = d + (realToFrac dy) / 50, cameraPosition = realToFrac <$> pos}
  | otherwise = c {cameraAngles = (newx, if inbounds newy then newy else y), cameraPosition = realToFrac <$> pos}
  where newx = x + (realToFrac dx) / 100
        newy = y + (realToFrac dy) / 100
        inbounds ny = ny > 0.1 && ny < 3.1415

draw :: Player -> EulerCamera -> [ModelData] -> Picture
draw _ cam models = Picture models cam

mouseDelta :: ((Int, Int), Bool) -> ((Int, Int), (Int, Int), Bool) -> ((Int, Int), (Int, Int), Bool)
mouseDelta ((x, y), pressed) (_, _, False) = ((x, y), (0, 0), True)
mouseDelta ((x, y), True) ((oldx, oldy), (dx, dy), True) = ((x, y), (oldx - x, oldy - y), True)
mouseDelta ((x, y), False) ((oldx, oldy), (dx, dy), True) = ((x, y), (0, 0), True)

loadObjModel :: Object -> IO ModelData
loadObjModel (Sun _ x y z) = do
  m <- loadModel "Resources/moon1.easy" "Resources/sun.jpg"
  return $ m {position = L.V3 x y z}
loadObjModel (Planet _ x y z) = do
  m <- loadModel "Resources/moon1.easy" "Resources/aaa.bmp"
  return $ m {position = L.V3 x y z}

loadSystemModels :: FilePath -> IO ([ModelData])
loadSystemModels path = do
  (System _ _ objs) <- loadSystem path
  mapM loadObjModel objs

moveObjects :: Float -> [ModelData] -> [ModelData]
moveObjects _ [] = []
moveObjects _ (sun:list) = (sun : map (move sun) list)
  where move sun obj@ModelData{ position = pos} = obj {position = rotate pos}
        rotate pos = L.rotate (eulerQuat ((L.V3 0 1 0) L.^/ 300)) pos

moveCat :: Player -> ModelData -> [ModelData]
moveCat (Player pos _) obj = [obj {position = pos}]

getTarget :: (Maybe Int) -> [ModelData] -> Maybe (L.V3 Float)
getTarget Nothing _ = Nothing
getTarget (Just x) list = case list ^? element x of
  Nothing -> Nothing
  Just (ModelData {position = pos}) -> Just pos


lastFlyCommand :: Maybe ConsoleCommand -> Maybe Int -> Maybe Int
lastFlyCommand (Just (Fly x)) _ = Just x
lastFlyCommand _ (Just x) = Just x
lastFlyCommand Nothing _ = Nothing
lastFlyCommand a _ = Nothing

-- map :: IO (MSygnal Picture)
gameMap deltasSygnal inputSygnal mouseRight commands mouse = do
  deltaInput <- nodeZip deltasSygnal inputSygnal
  systemModels <- loadSystemModels "Resources/Systems/Rooky.sys"
  cat <- loadModel "Resources/cat.easy" "Resources/body_diff.png"
  flyTo <- nodeFold lastFlyCommand commands Nothing
  system <- nodeFold moveObjects deltasSygnal systemModels
  target <- nodeZipWith getTarget flyTo system
  subplayer <- nodeZip3  deltasSygnal inputSygnal target
  player <- nodeFold update subplayer (Player (L.V3 3 0 0) 0)

  subdmouse <- nodeFold mouseDelta mouse (undefined, undefined, False)
  dmouse <- nodeMap (\(_, d, _) -> d) subdmouse
  subcam <- nodeZip3 player dmouse mouseRight
  cam <- nodeFold cameraFollow subcam $ EulerCamera 10 (L.V3 0 0 0) (1, 1)

  subres0 <- nodeConst cat
  subres1 <- nodeZipWith moveCat player subres0
  subres2 <- nodeZipWith (++) subres1 system
  nodeMap3 draw player cam subres2

main = runEngine $ do
  i <- asks input >>= lift
  inputSygnal <- askSubSygnal i Keyboard.onekey
  mouse <- liftM2 (nodeZipWith (,))
             (askSubSygnal i Mouse.position)
             (askSubSygnal i Mouse.isDownLeft) >>= lift
  mouseRight <- askSubSygnal i (Keyboard.isDownChar 'Z')
  dimensionsSygnal <- askSygnal dimensions
  deltasSygnal <- askSygnal deltas >>= lift
  commands <- askSygnal console >>= lift

  res <- lift $ gameMap deltasSygnal inputSygnal mouseRight commands mouse
  return (res :: MSygnal Picture)


