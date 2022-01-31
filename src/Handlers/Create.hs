{-# LANGUAGE LambdaCase #-}

module Handlers.Create where

import Data.Aeson (toJSON)
import Data.Text (unpack)
import Aws.Lambda (ApiGatewayRequest, Context, ApiGatewayResponse, apiGatewayRequestBody)
import Conduit (MonadIO)
import Control.Lens ((.~))
import Control.Lens.Lens ((&))
import Control.Monad (void)
import Handlers.Get (getTodoItem)
import Model (TodoItem (id, TodoItem), toAttributeValues)
import Network.AWS (MonadAWS, send)
import Network.AWS.DynamoDB (putItem)
import Network.AWS.DynamoDB.PutItem (piItem)
import System.Log.FastLogger (TimedFastLogger, newTimeCache, simpleTimeFormat, newTimedFastLogger, LogType' (LogStdout))
import System.Environment (getEnv)
import Lambda (success, badRequest, TableName (TableName), log, runAWSAction, LambdaEnv (LambdaEnv))
import Data.String (fromString)
import Data.Functor (($>))

createTodo :: (MonadAWS m, MonadIO m) => TimedFastLogger -> TableName -> TodoItem -> m (Either String ())
createTodo logger tn@(TableName table) item = do
    foundTodo <- getTodoItem logger tn itemId
    case foundTodo of
      Nothing ->
        Lambda.log logger ("Creating item: " <> (show . toJSON $ item)  <> " in table " <> unpack table <> ".") *>
        (void . send $ putItem table & piItem .~ toAttributeValues item) *>
        Lambda.log logger ("Created item: " <> (show . toJSON $ item)  <> " in table " <> unpack table <> ".") $>
        Right ()
      Just _ ->
        return . Left $ ("Item with id:" <> show itemId <> " already exists!")
    where itemId = Model.id item

createTodoHandler ::
    LambdaEnv ->
    ApiGatewayRequest TodoItem ->
    Context () ->
  IO (Either (ApiGatewayResponse String) (ApiGatewayResponse ()))
createTodoHandler lenv @ (LambdaEnv logger env tableName) req _ =
    case apiGatewayRequestBody req of
        Just body ->
            runAWSAction lenv (createTodo logger tableName body) >>= \case
                Right resp -> return . Right . success $ resp
                Left e -> return . Left . badRequest $ e 
        Nothing ->
            Lambda.log logger "missing Item body!" $> Left (badRequest "missing Item body!")