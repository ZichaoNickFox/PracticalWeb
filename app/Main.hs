module Main (main) where

import qualified Adapter.InMemory.Auth as M
import           Control.Exception
import           Control.Monad.Reader
import           Data.Default
import           Domain.Login
import           Domain.Registration
import           GHC.Conc
import           Katip
import           System.IO

newtype App a = App
  { unApp :: ReaderT (TVar M.State) (KatipContextT IO) a
  } deriving (Applicative, Functor, Monad, MonadReader (TVar M.State), MonadIO,
              KatipContext, Katip, MonadFail)

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

run :: LogEnv -> TVar M.State -> App a -> IO a
run le state = runKatipContextT le () mempty
  . flip runReaderT state
  . unApp

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

withKatip :: (LogEnv -> IO a) -> IO a
withKatip = bracket createLogEnv closeScribes
  where
    createLogEnv = do
      logEnv <- initLogEnv "HAuth" "prod"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

main :: IO ()
main = do
  withKatip $ \le -> do
    state <- newTVarIO (def :: M.State)
    run le state action
