{-# LANGUAGE ApplicativeDo, TypeApplications, OverloadedStrings #-}

module Main where

import           Data.Char
import           Data.Validation
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Control.Monad
import           Data.List.NonEmpty
import           Data.Coerce

newtype Password = Password T.Text deriving (Show, Eq)

newtype Error = Error (NonEmpty T.Text) deriving (Show, Eq)

newtype Username = Username T.Text deriving (Show, Eq)

instance Semigroup Error where
  Error xs <> Error ys = Error (xs <> ys)

generateError :: T.Text -> Error
generateError s = Error $ s :| []

checkLength :: Int -> Int -> Rule T.Text
checkLength min max x = case (pwLen > max || pwLen < min) of
  True -> Failure $ generateError $ T.concat
    [ "It is longer than "
    , T.pack $ show max
    , " or shorter than "
    , T.pack $ show min
    , " characters"
    ]

  False -> Success x
  where pwLen = T.length x

checkPasswordLength :: Rule Password
checkPasswordLength = coerce $ checkLength 11 19 :: Rule Password

checkUsernameLength :: Rule Username
checkUsernameLength = coerce $ checkLength 3 12 :: Rule Username


requireAlphaNum :: Rule T.Text
requireAlphaNum xs = case (all isAlphaNum $ T.unpack xs) of
  False ->
    Failure
      $ generateError
      $ "Your password has to be consisted of numeric or alpha character"
  True -> Success xs

type Rule a = (a -> Validation Error a)

cleanWhitespace :: Rule T.Text
cleanWhitespace "" = Failure $ generateError "Empty String is not allowed"
cleanWhitespace x  = Success $ T.strip x

validatePassword :: (Rule Password)
validatePassword password =
  case (coerce cleanWhitespace :: Rule Password) password of
    Failure err       -> Failure err
    Success password2 -> (coerce requireAlphaNum :: Rule Password) password2
      *> checkPasswordLength password2

validateUsername :: Username -> Validation Error Username
validateUsername username =
  (coerce cleanWhitespace :: Rule Username) username
    *> (coerce requireAlphaNum :: Rule Username) username
    *> checkUsernameLength username

passwordErrors :: Password -> Validation Error Password
passwordErrors password = case validatePassword password of
  Failure err       -> Failure (generateError "Invalid password:" <> err)
  Success password2 -> Success password2

usernameErrors :: Username -> Validation Error Username
usernameErrors username = case validateUsername username of
  Failure err       -> Failure (generateError "Invalid username:" <> err)
  Success username2 -> Success username2

reverseLine :: IO ()
reverseLine = do
  line <- getLine
  print $ Prelude.reverse line

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe (Just a) f = f a
bindMaybe Nothing  _ = Nothing

data StringOrValue a = Str T.Text | Val a deriving Show

bindStringOrValue
  :: StringOrValue a -> (a -> StringOrValue b) -> StringOrValue b
bindStringOrValue (Str s) _ = Str s
bindStringOrValue (Val s) f = f s

data User = User Username Password deriving Show

makeUserTmpPassword :: Username -> Validation Error User
makeUserTmpPassword username =
  User <$> validateUsername username <*> (pure $ Password "temporaryPassword")

makeUser :: Username -> Password -> Validation Error User
makeUser username password =
  User <$> usernameErrors username <*> passwordErrors password

promptWord :: T.Text -> IO T.Text
promptWord wordName =
  T.putStr ("Please enter a " <> wordName <> "\n") *> T.getLine

display :: Username -> Password -> IO ()
display name password = case makeUser name password of
  Failure err -> T.putStr $ T.unlines $ toList $ coerce err
  Success (User username _) ->
    T.putStrLn ("Welcome, " <> (coerce @Username @T.Text) username)

main :: IO ()
main =
  join
    $   display
    <$> (Username <$> promptWord "username")
    <*> (Password <$> promptWord "password")

  -- do
  -- username <- (Username <$> promptWord "username")
  -- password <- (Password <$> promptWord "password")
  -- display username password

  -- (display <$> (Username <$> promptWord "username"))
  --   <*> (Password <$> promptWord "password")

printTestResult :: Either T.Text () -> IO ()
printTestResult r = case r of
  Left  err -> T.putStrLn err
  Right ()  -> T.putStrLn "All tests passed"

eq :: (Eq a, Show a) => Int -> a -> a -> Either T.Text ()
eq n actual expected = case (actual == expected) of
  True  -> Right ()
  False -> Left
    (T.unlines
      [ "Test "
      , T.pack $ show n
      , " Expected:  "
      , T.pack $ show expected
      , " But got:   "
      , T.pack $ show actual
      ]
    )

test :: IO ()
test = printTestResult $ do
  eq
    1
    (checkPasswordLength (coerce @T.Text @Password ""))
    (Failure $ generateError
      "Your password cannot be longer than 19 or shorter than 11 characters"
    )
  eq 2
     (checkPasswordLength (coerce @T.Text @Password "julielovebooks"))
     (Success $ Password "julielovebooks")
