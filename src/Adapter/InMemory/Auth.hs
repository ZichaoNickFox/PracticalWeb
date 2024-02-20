module Adapter.InMemory.Auth where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Default
import           Data.Has
import           Data.List            (find)
import           Data.Map
import           Data.Set
import           Data.Text            (pack)
import           Domain.Login
import           Domain.Registration
import           GHC.Conc
import           Prelude              hiding (lookup, map)
import           Text.StringRandom

data State = State
  { stateAuths            :: [(UserId, Auth)]
  , stateUnverifiedEmails :: Map VerificationCode Email
  , stateVerifiedEmails   :: Set Email
  , stateUserIdCounter    :: Int
  , stateNotifications    :: Map Email VerificationCode
  , stateSessions         :: Map SessionId UserId
  } deriving (Show, Eq)

instance Default State where
  def = State
    { stateAuths = def :: [(UserId, Auth)]
      , stateUnverifiedEmails = def :: Map VerificationCode Email
      , stateVerifiedEmails = def :: Set Email
      , stateUserIdCounter = def :: Int
      , stateNotifications = def :: Map Email VerificationCode
      , stateSessions = def :: Map SessionId UserId
    }

type ImMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

addAuth :: ImMemory r m => Auth -> m (Either RegistrationError (UserId, VerificationCode))
addAuth auth = do
  tvar <- asks getter
  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  liftIO $ atomically $ runExceptT $ do
    state <- lift $ readTVar tvar
    -- check whether the given email is duplicate
    let auths = stateAuths state
        email = authEmail auth
        isDuplicate = elem email . fmap (authEmail . snd) $ auths
    when isDuplicate $ throwError RegistrationErrorEmailToken
    -- update the state
    let newUserId = stateUserIdCounter state + 1
        newAuths = (UserId newUserId, auth) : auths
        unverifieds = stateUnverifiedEmails state
        newUnverifieds = Data.Map.insert vCode email unverifieds
        newState = state
          { stateAuths = newAuths
          , stateUserIdCounter = newUserId
          , stateUnverifiedEmails = newUnverifieds
          }
    lift $ writeTVar tvar newState
    return (UserId newUserId, vCode)

setEmailAsVerified :: ImMemory r m => VerificationCode -> m (Either EmailVerificationErr (UserId, Email))
setEmailAsVerified vCode = do
  tvar <- asks getter
  liftIO $ atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let unverifieds = stateUnverifiedEmails state
        mayEmail = lookup vCode unverifieds
    email <- mayEmail `orThrow` EmailVerificationErrInvalidCode
    let auths = stateAuths state
        mayUserId = fmap fst . find ((email==) . authEmail . snd) $ auths
    userId <- mayUserId `orThrow` EmailVerificationErrInvalidCode
    let verifieds = stateVerifiedEmails state
        newVerifieds = Data.Set.insert email verifieds
        newUnverifieds = Data.Map.delete vCode unverifieds
        newState = state
          { stateUnverifiedEmails = newUnverifieds
          , stateVerifiedEmails = newVerifieds
          }
    lift $ writeTVar tvar newState
    return (userId, email)

findUserByAuth :: ImMemory r m => Auth -> m (Maybe (UserId, Bool))
findUserByAuth auth = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mayUserId = fmap fst $ find ((auth==) . snd) $ stateAuths state
  case mayUserId of
    Nothing -> return Nothing
    Just userId -> do
      let verifieds = stateVerifiedEmails state
          email = authEmail auth
          isVerified = email `elem` verifieds
      return $ Just (userId, isVerified)

findEmailFromUserId :: ImMemory r m => UserId -> m (Maybe Email)
findEmailFromUserId userId = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let myAuth = fmap snd $ find ((userId ==) . fst) (stateAuths state)
  return $ authEmail <$> myAuth

notifyEmailVerification :: ImMemory r m => Email -> VerificationCode -> m ()
notifyEmailVerification email vCode = do
  tvar <- asks getter
  liftIO $ atomically $ do
    state <- readTVar tvar
    let notifications = stateNotifications state
        newNotifications = Data.Map.insert email vCode notifications
        newState = state { stateNotifications = newNotifications }
    writeTVar tvar newState

getNotificationsForEmail :: ImMemory r m => Email -> m (Maybe VerificationCode)
getNotificationsForEmail email = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  return $ lookup email $ stateNotifications state

newSession :: ImMemory r m => UserId -> m SessionId
newSession userId = do
  tvar <- asks getter
  sessionId <- liftIO $ ((pack $ show userId) <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
  liftIO $ atomically $ do
    state <- readTVar tvar
    let sessions = stateSessions state
        newSessions = Data.Map.insert sessionId userId sessions
        newState = state { stateSessions = newSessions }
    writeTVar tvar newState
    return sessionId

findUserIdBySessionId :: ImMemory r m => SessionId -> m (Maybe UserId)
findUserIdBySessionId sessionId = do
  tvar <- asks getter
  liftIO $ lookup sessionId . stateSessions <$> readTVarIO tvar

orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow Nothing e  = throwError e
orThrow (Just a) _ = return a
