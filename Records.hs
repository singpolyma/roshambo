module Records where

import qualified Data.Text as T
import qualified Data.Aeson as Aeson

data ErrorMessage = ErrorMessage {
	errorMessage :: String
}

data GameContext = GameContext {
	winner :: Maybe String,
	finished :: Bool,
	choices :: [ChoiceRecord]
}

data ChoiceRecord = ChoiceRecord {
	email :: String,
	choice :: String
}

instance Aeson.ToJSON GameContext where
	toJSON (GameContext w f _) = Aeson.object [
			(T.pack "winner", Aeson.toJSON w),
			(T.pack "finished", Aeson.toJSON f)
		]
