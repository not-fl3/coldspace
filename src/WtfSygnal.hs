{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- {-# LANGUAGE ExistentialQuantification #-}

module WtfSygnal(
  Sygnal(..),
  nodeMap,
  nodeZip,
  nodeMap3,
  nodeZip3,
  nodeZipWith,
  nodeFold,
  nodeGetIO,
  nodeConst,
  runSygnal,
  Node(..), runNode,
  NodeDatable(..)) where

import Control.Applicative
import Data.Dynamic
import Data.Function
import Data.IORef

data NodeState a = Calced a | NotCalced deriving Show
data NodeList a next = Empty | (:.) a next deriving Show
data NodeType a = NodeContent a
                | NodeGetIO (IO (a))
                | NodeZipWith (a -> a -> a)
                | NodeFold (a -> a -> a) a
                | NodeMap (a -> a)
                | NodeMap3 (a -> a -> a -> a)

instance Show a => Show (NodeType a) where
  show (NodeContent x) = show x
  show (NodeGetIO _)   = "getIO"
  show (NodeZipWith _) = "zipWith"
  show (NodeMap _)     = "map"
  show (NodeMap3 _)     = "map"
  show (NodeFold _ _)  = "fold"

data NodeData a = NodeData (NodeState a) (NodeType a) deriving Show
data Sygnal a b = Sygnal (Node a)

-- instance Functor (Sygnal a) where
--   fmap f (Sygnal x) = Sygnal x

data Node a = Node (IORef (NodeData a)) [Node a]

class NodeDatable a b where
  toNodeData :: a -> b
  fromNodeData :: b -> a

nodeZip :: (NodeDatable a nodeData, NodeDatable b nodeData, NodeDatable (a, b) nodeData) =>
            Sygnal nodeData a -> Sygnal nodeData b -> IO (Sygnal nodeData (a, b))
nodeZip = nodeZipWith (,)

nodeZip3 :: (NodeDatable a nodeData, NodeDatable b nodeData, NodeDatable c nodeData, NodeDatable (a, b, c) nodeData) =>
                Sygnal nodeData a -> Sygnal nodeData b -> Sygnal nodeData c -> IO (Sygnal nodeData (a, b, c))
nodeZip3 = nodeMap3 (\a b c -> (a, b, c))

nodeZipWith :: (NodeDatable a nodeData, NodeDatable b nodeData, NodeDatable c nodeData) =>
                (a -> b -> c) -> Sygnal nodeData a -> Sygnal nodeData b -> IO (Sygnal nodeData c)
nodeZipWith f (Sygnal a) (Sygnal b) = do
    r <- newIORef $ NodeData NotCalced $ NodeZipWith (\x y -> toNodeData $ f (fromNodeData x) (fromNodeData y))
    return $ Sygnal $ Node r [a, b]

nodeMap :: (NodeDatable a nodeData, NodeDatable b nodeData) => (a -> b) -> Sygnal nodeData a -> IO (Sygnal nodeData b)
nodeMap f (Sygnal a) = do
    r <- newIORef $ NodeData NotCalced $ NodeMap (\x -> toNodeData $ f (fromNodeData x))
    return $ Sygnal $ Node r [a]

nodeMap3 :: (NodeDatable a nodeData, NodeDatable b nodeData, NodeDatable c nodeData, NodeDatable d nodeData) =>
  (a -> b -> c -> d) -> Sygnal nodeData a -> Sygnal nodeData b -> Sygnal nodeData c -> IO (Sygnal nodeData d)
nodeMap3 f (Sygnal a) (Sygnal b) (Sygnal c) = do
  r <- newIORef $ NodeData NotCalced $ NodeMap3 (\x y z -> toNodeData $ f (fromNodeData x) (fromNodeData y) (fromNodeData z))
  return $ Sygnal $ Node r [a, b, c]

nodeConst :: NodeDatable a nodeData => a -> IO (Sygnal nodeData a)
nodeConst a = do
  x <- newIORef $ NodeData NotCalced $ NodeContent (toNodeData a)
  return $ Sygnal $ Node x []

nodeGetIO :: NodeDatable a nodeData => IO a -> IO (Sygnal nodeData a)
nodeGetIO a = do
  x <- newIORef $ NodeData NotCalced $ NodeGetIO (toNodeData <$> a)
  return $ Sygnal $ Node x []

nodeFold :: (NodeDatable a nodeData, NodeDatable b nodeData, NodeDatable c nodeData) =>
            (a -> b -> c) -> Sygnal nodeData a -> b -> IO (Sygnal nodeData c)
nodeFold f (Sygnal syg) start = do
  r <- newIORef $ NodeData NotCalced (NodeFold (\x y -> toNodeData $ f (fromNodeData x) (fromNodeData y)) (toNodeData start) )
  return $ Sygnal $ Node r [syg]

addSome :: Int -> Int
addSome x = x + 1

pairySome :: Int -> Int -> Int
pairySome x y = x + y

foldySome :: Int -> Int -> Int
foldySome x y = x + y

runNode :: Node a  -> IO a
runNode startNode@(Node ref lst) = do
  setCalced startNode >> ((readIORef ref) >>= flip (oldRunNode ref) lst)

runSygnal :: Sygnal a b -> IO a
runSygnal (Sygnal startNode@(Node ref lst)) = do
  setCalced startNode >> ((readIORef ref) >>= flip (oldRunNode ref) lst)

setCalced (Node ref lst) = do
  modifyIORef ref (\(NodeData st x) -> (NodeData NotCalced x))
  mapM_ setCalced lst

oldRunNode :: IORef (NodeData a) -> NodeData a -> [Node a]-> IO a
oldRunNode ref (NodeData (Calced x) _) _ = return x
oldRunNode ref (NodeData st (NodeContent x)) [] = return x
oldRunNode ref (NodeData st (NodeContent x)) _  = error "content node cant have childs"

oldRunNode ref (NodeData st (NodeMap f)) (x:[]) = f <$> getFromNode x
oldRunNode ref (NodeData st (NodeMap x)) _      = error "map must have one argument"

oldRunNode ref (NodeData st (NodeMap3 f)) (x:y:z:[]) = f <$> getFromNode x <*> getFromNode y <*> getFromNode z
oldRunNode ref (NodeData st (NodeMap3 x)) _      = error "map3 must have 3 argument"

oldRunNode ref (NodeData st (NodeZipWith f)) (x:y:[]) = f <$> getFromNode x <*> getFromNode y
oldRunNode ref (NodeData st (NodeZipWith f)) _ = error "zip must have two arguments"

oldRunNode ref (NodeData st (NodeFold f b0)) (x:[]) = do
  b <- getFromNode x
  let res = f b b0
  writeIORef ref (NodeData (Calced res) (NodeFold f res))
  return res
oldRunNode ref (NodeData st (NodeFold f b0)) (lst) = error $ unlines ["fold must have one argument ",  show $ length lst]

oldRunNode ref (NodeData st (NodeGetIO io)) [] = do
  res <- io
  writeIORef ref $ NodeData (Calced res) (NodeGetIO io)
  return res
oldRunNode ref (NodeData st (NodeGetIO io)) _ = error "getIO must have 0 arguments"

getFromNode :: (Node a) -> IO a
getFromNode (Node r l) = readIORef r >>= flip (oldRunNode r) l
