module Sygnal where

import Control.Applicative
import Data.Function
import Debug.Trace

data Sygnal a = Sygnal {
  value :: (IO a),
  next  :: IO (Sygnal a)
  }

instance Functor Sygnal where
  fmap f (Sygnal x n) = Sygnal (fmap f x) $ (fmap . fmap) f n

instance Applicative Sygnal where
  pure x = Sygnal (return x) $ return $ pure x
  a@(Sygnal f n) <*> (Sygnal ax ~an) = Sygnal (f >>= flip fmap ax) (an >>= \(~anext) -> n >>= \fnext -> return $ fnext <*> anext)

sygnalFold :: (a -> b -> b) -> (Sygnal a) -> IO b -> (Sygnal b)
sygnalFold f a b = Sygnal b (next (return a) b)
  where next = fix $ \gen -> \aaa -> \bx ->
          do
            rbx <- bx
            return $ Sygnal (return rbx) $ do
              (Sygnal ax nx) <- aaa
              rax <- ax
              --rnx <- nx
              gen nx $ do
                return $ f rax rbx

sygnalZipWith :: (a -> b -> c) -> Sygnal a -> Sygnal b -> Sygnal c
sygnalZipWith f (Sygnal ax an) (Sygnal bx bn) = Sygnal (liftA2 f ax bx) $ liftA2 (sygnalZipWith f) an bn

sygnalConst :: a -> Sygnal a
sygnalConst x = Sygnal (return x) $ return $ sygnalConst x

sygnalGetN :: Int -> Sygnal a -> IO a
sygnalGetN 0 z  = value z
sygnalGetN n z = (value z >> next z) >>= sygnalGetN (n - 1)

wtfSygnal = Sygnal ((putStrLn "CURRENT") >> return 5) $ (putStrLn "NEXTTT!!!!" >> return wtfSygnal)

f x y = x + y

