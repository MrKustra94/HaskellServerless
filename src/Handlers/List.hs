module Handlers.List where

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
import Network.AWS.DynamoDB (srsItems)
import Network.AWS.DynamoDB.Scan (scan)
import Network.AWS.DynamoDB.Types (attributeValue, avS)
import Control.Lens.Operators ((?~))
import Data.Maybe (maybeToList)

listTodoItems :: (MonadAWS m, MonadIO m) => TimedFastLogger -> TableName -> m [TodoItem]
listTodoItems logger (TableName table) = do
    Lambda.log logger ("Fetching items in table " <> table <> ".")
    resp <- send $ scan table
    Lambda.log logger ("Fetched items in table " <> table <> ".")
    return $ view srsItems resp >>= maybeToList . toTodoItem

listTodosHandler ::
    LambdaEnv ->
    ApiGatewayRequest () ->
    Context () ->
    IO (Either (ApiGatewayResponse String) (ApiGatewayResponse [TodoItem]))
listTodosHandler lenv @ (LambdaEnv logger env tableName) req _ =
    Right . success <$> runAWSAction lenv (listTodoItems logger tableName)