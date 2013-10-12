module Records where

import qualified Data.Text as T
import qualified Data.Aeson as Aeson

import Data.Text.Buildable
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM)
import Text.Blaze.Html.Renderer.Text (renderHtmlBuilder)

instance Buildable (MarkupM a) where
	build = renderHtmlBuilder . fmap (const ())

data ErrorMessage = ErrorMessage {
	errorMessage :: String
}

data GameContext = GameContext {
	winner :: Maybe String,
	finished :: Bool,
	choices :: [ChoiceRecord],
	form :: Html
}

data ChoiceRecord = ChoiceRecord {
	email :: String,
	choice :: String
}

instance Aeson.ToJSON GameContext where
	toJSON (GameContext w f _ _) = Aeson.object [
			(T.pack "winner", Aeson.toJSON w),
			(T.pack "finished", Aeson.toJSON f)
		]
