module Domain.Registration where

-- ebook p55

import           Control.Monad.Except
import           Control.Monad.Trans.Class (lift)
import           Data.Text
import           Domain.Validation
import           Prelude                   hiding (length)
import           Text.Regex.PCRE.Heavy

data Auth = Auth {
  authEmail    :: Email,
  authPassword :: Password
} deriving (Show, Eq)

newtype Email = Email { rawEmail :: Text } deriving (Show, Eq, Ord)
mkEmail :: Text -> Either [EmailValidationErr] Email
mkEmail = validate Email
  [ regexMatches
    [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
    EmailValidationErrInvalidEmail
  ]

newtype Password = Password { rawPassword :: Text } deriving (Show, Eq)
mkPassword :: Text -> Either [PasswordValidationErr] Password
mkPassword rawPassword = validate Password
  [ lengthBetween 5 50 (PasswordValidationErrLength (length rawPassword))
  , regexMatches [re|\d|] PasswordValidationErrMustContainNumber
  , regexMatches [re|[A-Z]|] PasswordValidationErrMustContainUpperCase
  , regexMatches [re|[a-z]|] PasswordValidationErrMustContainLowerCase
  ] rawPassword

data EmailValidationErr = EmailValidationErrInvalidEmail
data PasswordValidationErr =
  PasswordValidationErrLength Int |
  PasswordValidationErrMustContainUpperCase |
  PasswordValidationErrMustContainLowerCase |
  PasswordValidationErrMustContainNumber
data EmailVerificationErr = EmailVerificationErrInvalidCode

data RegistrationError = RegistrationErrorEmailToken deriving (Show, Eq)

type VerificationCode = Text

newtype UserId = UserId Int deriving (Show, Eq)

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationErr ())
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => EmailVerificationNotify m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

{-
instance AuthRepo IO where
  addAuth (Auth email password) = do
    print $ "adding auth : " <> rawEmail email
    return $ Right "fake verification code"

instance EmailVerificationNotify IO where
  notifyEmailVerification email vCode =
    print $ "notify : " <> rawEmail email <> " - " <> vCode
-}

register :: (AuthRepo m, EmailVerificationNotify m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  vCode <- ExceptT $ addAuth auth
  lift $ notifyEmailVerification (authEmail auth) vCode

verifyEmail :: AuthRepo m => VerificationCode -> m (Either EmailVerificationErr ())
verifyEmail = setEmailAsVerified
