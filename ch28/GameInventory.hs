{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GameInventory where


import Control.Concurrent.STM
import Control.Monad
import Data.List (delete)

data Item = Scroll
          | Wand
          | Banjo
            deriving (Eq, Ord, Show)

newtype Gold = Gold Int
    deriving (Eq, Ord, Show, Num)

newtype HitPoint = HitPoint Int
    deriving (Eq, Ord, Show, Num)

type Inventory = TVar [Item]
type Health = TVar HitPoint
type Balance = TVar Gold

data Player = Player {
      balance :: Balance,
      health :: Health,
      inventory :: Inventory
    }

basicTransfer :: Gold -> Balance -> Balance -> STM ()
basicTransfer qty fromBal toBal = do
    fromQty <- readTVar fromBal
    toQty <- readTVar toBal
    writeTVar fromBal (fromQty - qty)
    writeTVar toBal (toQty + qty)

transferTest = do
    alice <- newTVar (5 :: Gold)
    bob <- newTVar (12 :: Gold)
    basicTransfer 2 bob alice
    liftM2 (,) (readTVar alice) (readTVar bob)

removeInv :: Eq a => a -> [a] -> Maybe [a]
removeInv x xs = if x `elem` xs then Just $ x `delete` xs else Nothing

maybeGiveItem :: Item -> Inventory -> Inventory -> STM Bool
maybeGiveItem item fromInv toInv = do
    fromItems <- readTVar fromInv
    case removeInv item fromItems of
        Nothing -> return False
        Just remain -> do
            toItems <- readTVar toInv
            writeTVar fromInv remain
            writeTVar toInv (item :toItems)
            return True
