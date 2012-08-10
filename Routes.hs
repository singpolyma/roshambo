module Routes where

import Application
import Control.Monad (ap)
import Data.Text (pack)
import Web.PathPieces (fromPathPiece)
import Yesod.Routes.Dispatch (Route(..), Piece(Static, Dynamic))

routes arg1 = [
		Route {
			rhPieces = [Static (pack "GET")],
			rhHasMulti = False,
			rhDispatch = (\(_:_) -> (return $ home arg1))
		},
		Route {
			rhPieces = [Static (pack "GET"),Static (pack "game"),Dynamic],
			rhHasMulti = False,
			rhDispatch = (\(_:_:val0:_) -> (return $ showGame arg1) `ap` (fromPathPiece val0))
		},
		Route {
			rhPieces = [Static (pack "POST"),Static (pack "game"),Dynamic],
			rhHasMulti = False,
			rhDispatch = (\(_:_:val0:_) -> (return $ createChoice arg1) `ap` (fromPathPiece val0))
		}
	]
