{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Bank.Data (program) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    newTVarIO,
    readTVar,
    readTVarIO,
    retry,
    writeTVar,
  )
import Control.Monad (forever)
import Data.Semigroup (Sum (..))
import System.Exit (exitSuccess)

type Account = TVar Amount

newtype Amount = Amount Double
  deriving (Show, Eq, Ord, Num)
  deriving (Semigroup, Monoid) via (Sum Double)

mkAmount :: Double -> Amount
mkAmount = Amount

mkAccount :: Amount -> IO Account
mkAccount = newTVarIO

zero :: IO Account
zero = mkAccount mempty

credit :: Amount -> Account -> STM ()
credit amount account = do
  current <- readTVar account
  if amount > mempty
    then writeTVar account (current <> amount)
    else pure ()

debit :: Amount -> Account -> STM ()
debit amount account = do
  current <- readTVar account
  if current >= amount
    then writeTVar account (current - amount)
    else retry

transfer :: Amount -> Account -> Account -> STM ()
transfer amount from to = do
  debit amount from
  credit amount to

program :: IO ()
program = do
  let n = 10
  bob <- (mkAccount . mkAmount) 10000.0
  joe <- (mkAccount . mkAmount) 4000.0
  repeat n $ forkIO $ atomically $ transfer (mkAmount 500) bob joe
  forever $ do
    bobBalance <- readTVarIO bob
    joeBalance <- readTVarIO joe
    putStrLn ("Bob's balance: " <> show bobBalance <> ", Joe's balance: " <> show joeBalance)
    if bobBalance <= 8000
      then exitSuccess
      else putStrLn "Trying again."
  where
    repeat :: Integer -> IO a -> IO a
    repeat 1 m = m
    repeat n m = m >> repeat (n - 1) m
