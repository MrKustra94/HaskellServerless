module Lambda where

import Aws.Lambda (ApiGatewayResponse(..))
import Conduit ( runResourceT, MonadUnliftIO, MonadIO (liftIO) )
import Control.Monad.Catch (MonadCatch)
import Data.Aeson (ToJSON)
import qualified Data.Binary.Builder as BB
import Data.String (fromString)
import Network.AWS (AWS, Env, LogLevel, newEnv, Credentials(Discover), runAWS, HasEnv (envLogger))
import System.Log.FastLogger (TimedFastLogger, ToLogStr (toLogStr), newTimeCache, simpleTimeFormat, LogType' (LogStdout), newTimedFastLogger)
import Data.Text (Text)
import Data.Binary.Builder (toLazyByteString)
import System.Environment (getEnv)
import Control.Lens ((.~))
import Control.Lens.Lens ((&))

newtype TableName = TableName Text deriving (Eq, Show)

data LambdaEnv = LambdaEnv {
  logger    :: TimedFastLogger,
  env       :: Env,
  tableName :: TableName
}

makeDefaultEnv :: IO LambdaEnv
makeDefaultEnv = do
  timeCache <- newTimeCache simpleTimeFormat
  (logger, _) <- newTimedFastLogger timeCache (LogStdout 1)
  tableName <- getEnv "DYNAMODB_TABLE"
  env <- newEnv Discover
  return LambdaEnv {
    logger = logger,
    env = env & envLogger .~ tfLogger logger,
    tableName = TableName . fromString $ tableName
  }

log :: (Show a, MonadIO m) => TimedFastLogger -> a -> m ()
log logger msg = liftIO $ logger (\time -> toLogStr (show time <> " " <> show msg <> "\n"))

tfLogger :: TimedFastLogger -> LogLevel -> BB.Builder -> IO ()
tfLogger tfl logLevel builder =
  Lambda.log tfl (toLazyByteString (logLevelBuilder `BB.append` builder))
  where logLevelBuilder = BB.putStringUtf8 ("[" ++ show logLevel ++ "] ")

runAWSAction :: (MonadCatch m, MonadUnliftIO m) => LambdaEnv -> AWS b -> m b
runAWSAction lenv action = runResourceT $ runAWS (env lenv) action

success :: ToJSON a => a -> ApiGatewayResponse a
success a =
    ApiGatewayResponse {
      apiGatewayResponseStatusCode = 200,
      apiGatewayResponseHeaders = [],
      apiGatewayResponseBody = a,
      apiGatewayResponseIsBase64Encoded = False
    }

badRequest :: String -> ApiGatewayResponse String
badRequest msg =
    ApiGatewayResponse {
      apiGatewayResponseStatusCode = 400,
      apiGatewayResponseHeaders = [],
      apiGatewayResponseBody = msg,
      apiGatewayResponseIsBase64Encoded = False
    }

notFound::String -> ApiGatewayResponse String
notFound msg =
    ApiGatewayResponse {
      apiGatewayResponseStatusCode = 404,
      apiGatewayResponseHeaders = [],
      apiGatewayResponseBody = msg,
      apiGatewayResponseIsBase64Encoded = False
    }