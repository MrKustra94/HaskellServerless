{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Model where

import Aws.Lambda (ApiGatewayRequest, apiGatewayRequestPathParameters)
import Control.Lens (view, (?~))
import Control.Lens.Lens ((&))
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.AWS.DynamoDB (attributeValue, avS)
import Network.AWS.DynamoDB.Types (AttributeValue)
import Network.AWS.Prelude (HashMap)

newtype ItemId = ItemId Text deriving (Eq, Show, Generic, FromJSON, ToJSON)

idToText :: ItemId -> Text 
idToText (ItemId text) = text

newtype ItemDescription = ItemDescription Text deriving (Eq, Show, Generic, FromJSON, ToJSON)
descriptionToText :: ItemDescription -> Text 
descriptionToText (ItemDescription text) = text

data TodoItem = TodoItem { id:: ItemId, description:: ItemDescription } deriving (Eq, Show, Generic, FromJSON, ToJSON)

idFromRequest :: ApiGatewayRequest a -> Maybe ItemId
idFromRequest rq =
    apiGatewayRequestPathParameters rq >>= \params ->
        fmap ItemId (HM.lookup "id" params)

toAttributeValues :: TodoItem -> HashMap Text AttributeValue
toAttributeValues (TodoItem (ItemId id) (ItemDescription desc)) = HM.fromList [
    ("Id", attributeValue & avS ?~ id),
    ("Description", attributeValue & avS ?~ desc)]

toTodoItem :: HashMap Text AttributeValue -> Maybe TodoItem
toTodoItem vals = do
    idAV <- HM.lookup "Id" vals
    id <- view avS idAV
    descAV <- HM.lookup "Description" vals
    desc <- view avS descAV
    return TodoItem { id = ItemId id, description = ItemDescription desc}