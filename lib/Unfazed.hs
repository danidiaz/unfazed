{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Unfazed
  ( StoreName,
    newStore,
    queryStore,
    StoreNotFound (..),
    TypeMismatch (..),
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
newStore storeName value = do
  let actualType = typeRep (Proxy @a)
  let loop = do
        e <- try @Query sleepForever
        -- TODO: There might be async exception unsafety here.
        case e of
          Left Query {queryThreadId, queryType} -> do
            if queryType == actualType
              then throwTo queryThreadId Response {response = value}
              else throwTo queryThreadId TypeMismatch {storeName, queryType, actualType}
            loop
          Right _ -> error "impossible, we sleep forever!"
  loopyId <- forkIO loop
  _ <- labelThread loopyId storeName
  pure ()

queryStore :: forall a. (Typeable a) => StoreName -> IO a
queryStore name = do
  storeId <- findStore name
  queryThreadId <- myThreadId
  let queryType = typeRep (Proxy @a)
  e <- try @(Response a) do
    throwTo storeId Query {queryThreadId, queryType}
    sleepForever
  case e of
    Left (Response {response}) -> pure response
    Right _ -> error "impossible, we sleep forever!"

findStore :: StoreName -> IO ThreadId
findStore storeName = do
  threadList <- listThreads
  let go = \case
        [] -> throwIO StoreNotFound {storeName}
        threadId : threadIds -> do
          mlabel <- threadLabel threadId
          case mlabel of
            Just someName | someName == storeName -> pure threadId
            _ -> go threadIds
  go threadList

sleepForever :: IO x
sleepForever = forever (threadDelay maxBound)

data Query = Query {queryThreadId :: ThreadId, queryType :: TypeRep}
  deriving stock (Show)

instance Exception Query where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

data TypeMismatch = TypeMismatch
  { storeName :: StoreName,
    queryType :: TypeRep,
    actualType :: TypeRep
  }
  deriving stock (Show)

instance Exception TypeMismatch where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

data Response a = Response {response :: a}

instance Show (Response a) where
  show Response {} = "Response"

instance (Typeable a) => Exception (Response a) where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

data StoreNotFound = StoreNotFound {storeName :: StoreName}
  deriving stock (Show)
  deriving anyclass (Exception)
