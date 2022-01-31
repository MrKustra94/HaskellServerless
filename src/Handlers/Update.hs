{-# LANGUAGE LambdaCase #-}

module Handlers.Update where

import Data.Aeson (toJSON)
import Data.Text (unpack)
import Aws.Lambda (ApiGatewayRequest, Context, ApiGatewayResponse, apiGatewayRequestBody)
import Conduit (MonadIO)
import Control.Lens ((.~))
import Control.Lens.Lens ((&))
import Control.Monad (void)
import Handlers.Get (getTodoItem)
import Model (TodoItem (id, TodoItem, description), toAttributeValues, ItemId, ItemDescription, idToText, descriptionToText)
import Network.AWS (MonadAWS, send)
import Network.AWS.DynamoDB.UpdateItem (updateItem, uiKey, uiExpressionAttributeValues, uiUpdateExpression)
import System.Log.FastLogger (TimedFastLogger, newTimeCache, simpleTimeFormat, newTimedFastLogger, LogType' (LogStdout))
import System.Environment (getEnv)
import Lambda (success, badRequest, TableName (TableName), log, runAWSAction, LambdaEnv (LambdaEnv), notFound)
import Data.String (fromString)
import Data.Functor (($>))
import qualified Data.HashMap.Internal.Strict as HM
import Network.AWS.DynamoDB (attributeValue)
import Network.AWS.DynamoDB.Types (avS)
import Control.Lens.Operators ((?~))

newtype ItemDoesNotExistError = ItemDoesNotExistError ItemId deriving (Show, Eq)

toResponse :: ItemDoesNotExistError -> ApiGatewayResponse String
toResponse (ItemDoesNotExistError id) =
    notFound $ "Item : " <> show id <> " does not exist."

updateTodo :: (MonadAWS m, MonadIO m) => TimedFastLogger -> TableName -> TodoItem -> m (Either ItemDoesNotExistError ())
updateTodo logger tn@(TableName table) item@(TodoItem id desc)  = do
    foundTodo <- getTodoItem logger tn itemId
    case foundTodo of
      Nothing ->
        return . Left . ItemDoesNotExistError $ itemId
      Just _ ->
        Lambda.log logger ("Updating item: " <> (show . toJSON $ item)  <> " in table " <> unpack table <> ".") *>
        (void . send $ updateItem table
            & uiKey .~ HM.fromList [("Id", attributeValue & avS ?~ idToText id)]
            & uiExpressionAttributeValues .~ HM.fromList [(":Desc", attributeValue & avS ?~ descriptionToText desc)]
            & uiUpdateExpression ?~ "SET Descritpion = :Desc" ) *>
        Lambda.log logger ("Updated item: " <> (show . toJSON $ item)  <> " in table " <> unpack table <> ".") $>
        Right ()
    where itemId = Model.id item

updateTodoHandler ::
    LambdaEnv ->
    ApiGatewayRequest TodoItem ->
    Context () ->
  IO (Either (ApiGatewayResponse String) (ApiGatewayResponse ()))
updateTodoHandler lenv @ (LambdaEnv logger env tableName) req _ =
    case apiGatewayRequestBody req of
        Just body ->
            runAWSAction lenv (updateTodo logger tableName body) >>= \case
                Right resp -> return . Right . success $ resp
                Left e -> return . Left . toResponse $ e
        Nothing ->
            Lambda.log logger "missing Item body!" $> Left (badRequest "missing Item body!")