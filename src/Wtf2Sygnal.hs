{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
import Data.Maybe

data Folds = Folds [(Int, String)]
data Inputs = Inputs [(Int, String)]
  
class Foldy a where
  serialize :: a -> String
  deserialize :: String -> a

data Sygnal a = Sygnal a Folds

instance Functor Sygnal where
  fmap :: (a -> b) -> Sygnal a -> Sygnal b
  fmap f (Sygnal x map) = Sygnal (f x) map

instance Applicative Sygnal where
  pure = undefined
  f <*> x = undefined

instance Monad Sygnal where
  return = pure
  ma >>= mf = undefined

sygFold :: Foldy b => Int -> (b -> a -> b) -> Sygnal a -> b -> Sygnal b
sygFold n f a@(Sygnal _ (Folds folds)) b0 = f b <$> a
  where b = case lookup n folds of
              Nothing -> b0
              Just x -> deserialize x

input :: IO Int
input = getLine >>= return . read

runSygnal :: Sygnal a -> a
runSygnal = undefined


test = do
  undefined
