{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Unfazed
  ( newStore,
    queryStore,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Typeable
import GHC.Conc
import GHC.Conc.Sync (threadLabel)

type StoreName = String

newStore :: forall a. (Typeable a) => StoreName -> a -> IO ()
newStore name value = do
  let rep = typeRep (Proxy @a)
  let loop = do
        e <- try @Query sleepForever
        case e of
          Left Query {queryThreadId, queryTypeRep} -> do
            if queryTypeRep == rep
              then throwTo queryThreadId Response {response = value}
              else throwTo queryThreadId QueryError
          Right () -> loop
  loopyId <- forkIO loop
  _ <- labelThread loopyId name
  pure ()

queryStore :: forall a. (Typeable a) => StoreName -> IO a
queryStore name = do
  storeId <- findStore name
  queryThreadId <- myThreadId
  let queryTypeRep = typeRep (Proxy @a)
  e <- try @(Response a) do
    throwTo storeId Query {queryThreadId, queryTypeRep}
    sleepForever
  case e of
    Left (Response {response}) -> pure response
    Right _ -> error "impossible, we sleep forever!"

findStore :: StoreName -> IO ThreadId
findStore name = do
  threadList <- listThreads
  let go = \case
        [] -> throwIO StoreNotFound
        threadId : threadIds -> do
          mlabel <- threadLabel threadId
          case mlabel of
            Just someName | someName == name -> pure threadId
            _ -> go threadIds
  go threadList

sleepForever :: IO x
sleepForever = forever (threadDelay maxBound)

data Query = Query {queryThreadId :: ThreadId, queryTypeRep :: TypeRep}
  deriving stock (Show)
  deriving anyclass (Exception)

data QueryError = QueryError
  deriving stock (Show)
  deriving anyclass (Exception)

data Response a = Response {response :: a}
  deriving anyclass (Exception)

instance Show (Response a) where
  show Response {} = "Response"

data StoreNotFound = StoreNotFound
  deriving stock (Show)
  deriving anyclass (Exception)
