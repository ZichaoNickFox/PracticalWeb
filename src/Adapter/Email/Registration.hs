module Adapter.Email.Registration where

-- p55
import Data.Text

data Auth = Auth {
  authEmail :: Email,
  authPassword :: Password
} deriving (Show, Eq)

newtype Email = Email { emailRaw :: Text } deriving (Show, Eq)
mkEmail :: Text -> Either [EmailValidationErr] Email
mkEmail = undefined

newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)
mkPassword :: Text -> Either [PasswordValidationErr] Password
mkPassword = undefined

data EmailValidationErr = EmailValidationErrInvalidEmail
data PasswordValidationErr =
  PasswordValidationErrLength Int |
  PasswordValidationErrMustContainUpperCase |
  PasswordValidationErrMustContainLowerCase |
  PasswordValidationErrMustContainNumber 

data RegistrationError = RegistrationErrorEmailToken deriving (Show, Eq)

