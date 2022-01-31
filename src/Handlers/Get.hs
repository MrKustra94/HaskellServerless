{-# LANGUAGE LambdaCase #-}

module Handlers.Get where

import Data.Aeson (toJSON)
import qualified Data.HashMap.Internal.Strict as HM
import Data.Text (unpack)
import Aws.Lambda (ApiGatewayRequest, Context, ApiGatewayResponse, apiGatewayRequestBody)
import Conduit (MonadIO)
import Control.Lens ((.~), view)
import Control.Lens.Lens ((&))
import Control.Monad (void)
import Model (TodoItem, toAttributeValues, ItemId (ItemId), idFromRequest, toTodoItem)
import Network.AWS (MonadAWS, send)
import System.Log.FastLogger (TimedFastLogger, newTimeCache, simpleTimeFormat, newTimedFastLogger, LogType' (LogStdout), defaultBufSize)
import System.Environment (getEnv)
import Lambda (success, badRequest, notFound, TableName (TableName), log, runAWSAction, LambdaEnv (LambdaEnv))
import Data.String (fromString)
import Data.Functor (($>))
import Network.AWS.DynamoDB (qExpressionAttributeValues, qrsItems)
import Network.AWS.DynamoDB.Query (query, qKeyConditionExpression, qLimit)
import Network.AWS.DynamoDB.Types (attributeValue, avS)
import Control.Lens.Operators ((?~))
import Safe (headMay)

getTodoItem :: (MonadAWS m, MonadIO m) => TimedFastLogger -> TableName -> ItemId -> m (Maybe TodoItem)
getTodoItem logger (TableName table) (ItemId id) = do
    Lambda.log logger ("Fetching items for id: " <> id <> " in table " <> table <> ".")
    resp <- send $ query table
        & qExpressionAttributeValues .~ HM.fromList [(":id", attributeValue & avS ?~ id)]
        & qKeyConditionExpression ?~ "Id = :id"
        & qLimit ?~ 1
    Lambda.log logger ("Fetched items for id: " <> id <> " in table " <> table <> ".")    
    let itemsHead = headMay $ view qrsItems resp
    return $ itemsHead >>= toTodoItem

getTodoHandler ::
    LambdaEnv ->
    ApiGatewayRequest () ->
    Context () ->
    IO (Either (ApiGatewayResponse String) (ApiGatewayResponse TodoItem))
getTodoHandler lenv @ (LambdaEnv logger env tableName) req _ =
    case idFromRequest req of
        Just id ->
            runAWSAction lenv (getTodoItem logger tableName id) >>= \case
                Just item -> pure (Right (success item))
                Nothing -> pure (Left (notFound "Item not found!"))
        Nothing ->
            Lambda.log logger "missing valid id" $> Left (badRequest "Missing ID in request.")