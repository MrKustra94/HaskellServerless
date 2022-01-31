module Main where

import Aws.Lambda
import Handlers.Get
import Lambda (makeDefaultEnv)

main :: IO ()
main = do
  env <- makeDefaultEnv
  runLambdaHaskellRuntime
    defaultDispatcherOptions
    (pure ())
    id $ do
      -- You could also register multiple handlers
      addAPIGatewayHandler "handler" (getTodoHandler env)

