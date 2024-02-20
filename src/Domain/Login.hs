module Domain.Login where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import           Data.Text
import           Domain.Registration
import           Katip

type SessionId = Text

data LoginError =
    LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified deriving (Show, Eq)

class (Monad m, Control.Monad.IO.Class.MonadIO m) => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

{-
instance SessionRepo IO
-}

login :: (KatipContext m, AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing          -> throwError LoginErrorInvalidAuth
    Just (_, False)  -> throwError LoginErrorEmailNotVerified
    Just (userId, _) -> withUserIdContext userId . lift $ do
      sessionId <- newSession userId
      $(logTM) InfoS $ ls (rawEmail $ authEmail auth) <> " logged in successfully"
      return sessionId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId
