module Handlers.Delete where

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
import Network.AWS.DynamoDB.DeleteItem (deleteItem, diKey)
import Network.AWS.DynamoDB.Types (attributeValue, avS)
import Control.Lens.Operators ((?~))
import Safe (headMay)

deleteTodoItem :: (MonadAWS m, MonadIO m) => TimedFastLogger -> TableName -> ItemId -> m ()
deleteTodoItem logger (TableName table) (ItemId id) = do
    Lambda.log logger ("Deleting items for id: " <> id <> "in table " <> table <> ".")
    resp <- send $ deleteItem table
        & diKey .~ HM.fromList [("Id", attributeValue & avS ?~ id)]
    Lambda.log logger ("Deleted items for id: " <> id <> "in table " <> table <> ".")    

deleteTodoHandler ::
    LambdaEnv ->
    ApiGatewayRequest () ->
    Context () ->
    IO (Either (ApiGatewayResponse String) (ApiGatewayResponse ()))
deleteTodoHandler lenv @ (LambdaEnv logger env tableName) req _ =
    case idFromRequest req of
        Just id ->
            fmap (Right . success) (runAWSAction lenv (deleteTodoItem logger tableName id))
        Nothing ->
            Lambda.log logger "missing valid id" $> Left (badRequest "Missing ID in request.")