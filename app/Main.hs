module Main (main) where

import qualified Adapter.InMemory.Auth as M
import           Control.Monad.Reader
import           Data.Default
import           Domain.Login
import           Domain.Registration
import           GHC.Conc

newtype App a = App
  { unApp :: ReaderT (TVar M.State) IO a
  } deriving (Applicative, Functor, Monad, MonadReader (TVar M.State), MonadIO, MonadFail)

instance AuthRepo App where
  addAuth = M.addAuth
  setEmailAsVerified = M.setEmailAsVerified
  findUserByAuth = M.findUserByAuth
  findEmailFromUserId = M.findEmailFromUserId

instance EmailVerificationNotify App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  newSession = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId

run :: TVar M.State -> App a -> IO a
run state = flip runReaderT state . unApp

action :: App ()
action = do
  let email = either undefined id $ mkEmail "test@example.com"
      password = either undefined id $ mkPassword "123456ABCDefgh"
      auth = Auth email password
  _ <- register auth
  Just vCode <- M.getNotificationsForEmail email
  _ <- verifyEmail vCode
  Right session <- login auth
  Just userId <- resolveSessionId session
  Just registeredEmail <- getUser userId
  liftIO $ print (session, userId, registeredEmail)

main :: IO ()
main = do
  state <- newTVarIO (def :: M.State)
  run state action
