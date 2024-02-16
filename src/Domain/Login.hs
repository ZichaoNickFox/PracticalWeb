module Domain.Login where

import Data.Text
import Domain.Registration
import Control.Monad.Except
import Control.Monad.IO.Class

type SessionId = Text

data LoginError =
    LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified deriving (Show, Eq)

class (Monad m, Control.Monad.IO.Class.MonadIO m) => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

instance SessionRepo IO
  
login :: (AuthRepo m, SessionRepo m, Control.Monad.IO.Class.MonadIO m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- liftIO $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (userId, _) -> liftIO $ newSession userId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId

