{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module MyNodeData where

import           Engine.Engine
import           Engine.ConsoleCommand
import           Engine.Sygnals.Input
import           Graphics.Rendering.OpenGL
import qualified Linear as L
import           WtfSygnal

data Player = Player {
  playerPosition :: L.V3 Float,
  playerTarget :: Int
  }

data MyNodeData = NInt Int
                | NBool Bool
                | NFloat Float
                | NString String
                | NPair (MyNodeData, MyNodeData)
                | NList [MyNodeData]
                | NMaybe (Maybe MyNodeData)
                | NTriple (MyNodeData, MyNodeData, MyNodeData)
                | NPlayer Player
                | NCamera EulerCamera
                | NConsoleCommand ConsoleCommand
                | NModel ModelData
                | NPicture Picture
                | NKey Key
                | NLV3Float (L.V3 Float)
                | NInput1 Input
                | NDimensions (Int, Int)
                | NFail

instance Renderable MyNodeData where
  getPicture (NPicture pic) = pic
  getPicture (NFail)        = error "FAIL"
  getPicture _              = error "net ended with invalid type"

instance (NodeDatable a MyNodeData, NodeDatable b MyNodeData) => NodeDatable (a, b) MyNodeData where
  toNodeData (x, y) = NPair (toNodeData x, toNodeData y)
  fromNodeData (NPair (x, y)) = (fromNodeData x, fromNodeData y)
  fromNodeData _ = error "O_o invalid type (Pair)"

instance (NodeDatable a MyNodeData, NodeDatable b MyNodeData, NodeDatable c MyNodeData) => NodeDatable (a, b, c) MyNodeData where
  toNodeData (x, y, z) = NTriple (toNodeData x, toNodeData y, toNodeData z)
  fromNodeData (NTriple (x, y, z)) = (fromNodeData x, fromNodeData y, fromNodeData z)
  fromNodeData _ = error "O_o invalid type (Pair)"

instance NodeDatable a MyNodeData => NodeDatable [a] MyNodeData where
  toNodeData list = NList $ map toNodeData list
  fromNodeData (NList list) = map fromNodeData list
  fromNodeData _ = error "O_o invalid type (NList)"

instance NodeDatable a MyNodeData => NodeDatable (Maybe a) MyNodeData where
  toNodeData Nothing = NMaybe Nothing
  toNodeData (Just x) = NMaybe $ Just $ toNodeData x
  fromNodeData (NMaybe Nothing) = Nothing
  fromNodeData (NMaybe (Just x)) = Just $ fromNodeData x
  fromNodeData _ = error "O_o invalid type (NMaybe)"

instance NodeDatable ConsoleCommand MyNodeData where
  toNodeData x = NConsoleCommand x
  fromNodeData (NConsoleCommand x) = x
  fromNodeData _ = error "O_o invalid type Console"

instance NodeDatable Player MyNodeData where
  toNodeData x = NPlayer x
  fromNodeData (NPlayer x) = x
  fromNodeData _ = error "O_o invalid type Player"

instance NodeDatable Bool MyNodeData where
  toNodeData x = NBool x
  fromNodeData (NBool x) = x
  fromNodeData _ = error "O_o invalid type Input"

instance NodeDatable Input MyNodeData where
  toNodeData x = NInput1 x
  fromNodeData (NInput1 x) = x
  fromNodeData _ = error "O_o invalid type Input"

instance NodeDatable Int MyNodeData where
  toNodeData x = NInt x
  fromNodeData (NInt x) = x
  fromNodeData _ = error "O_o invalid type Int"

instance NodeDatable Float MyNodeData where
  toNodeData x = NFloat x
  fromNodeData (NFloat x) = x
  fromNodeData _ = error "O_o invalid type Float"

instance NodeDatable (L.V3 Float) MyNodeData where
  toNodeData x = NLV3Float x
  fromNodeData (NLV3Float x) = x
  fromNodeData _ = error "O_o invalid type Key"

instance NodeDatable Key MyNodeData where
  toNodeData x = NKey x
  fromNodeData (NKey x) = x
  fromNodeData _ = error "O_o invalid type Key"

instance NodeDatable EulerCamera MyNodeData where
  toNodeData x = NCamera x
  fromNodeData (NCamera x) = x
  fromNodeData _ = error "O_o invalid type GLCamera"

instance NodeDatable ModelData MyNodeData where
  toNodeData x = NModel x
  fromNodeData (NModel x) = x
  fromNodeData _ = error "O_o invalid type ModelData"

instance NodeDatable Picture MyNodeData where
  toNodeData x = NPicture x
  fromNodeData (NPicture x) = x
  fromNodeData _ = error "O_o invalid type Picture"
