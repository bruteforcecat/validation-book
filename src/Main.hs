{-# LANGUAGE ApplicativeDo, GeneralisedNewtypeDeriving, OverloadedStrings #-}

module Main where

import           Data.Char
import           Data.Validation
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

newtype Password = Password T.Text deriving (Show, Eq)

newtype Error = Error T.Text deriving (Show, Eq)

newtype Username = Username T.Text deriving (Show, Eq)

instance Semigroup Error where
  Error xs <> Error ys = Error (xs <> ys)

checkLength :: Int -> Int -> T.Text -> Validation Error T.Text
checkLength min max x = case (pwLen > max || pwLen < min) of
  True -> Failure $ Error $ T.unlines
    [ "It is <longer than "
    , T.pack $ show max
    , " or shorter than "
    , T.pack $ show min
    , " characters"
    ]
  False -> Success x
  where pwLen = T.length x

checkPasswordLength :: T.Text -> Validation Error Password
checkPasswordLength = fmap Password . checkLength 11 19

checkUsernameLength :: T.Text -> Validation Error Username
checkUsernameLength = fmap Username . checkLength 3 12


requireAlphaNum :: T.Text -> Validation Error T.Text
requireAlphaNum xs = case (all isAlphaNum $ T.unpack xs) of
  False -> Failure
    $ Error "Your password has to be consisted of numeric or alpha character"
  True -> Success xs

cleanWhitespace :: T.Text -> Validation Error T.Text
cleanWhitespace "" = Failure $ Error "Empty String is not allowed"
cleanWhitespace x  = Success $ T.strip x

validatePassword :: Password -> Validation Error Password
validatePassword (Password password) = case (cleanWhitespace password) of
  Failure err -> Failure err
  Success password2 ->
    requireAlphaNum password2 *> checkPasswordLength password2

validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  cleanWhitespace username
    *> requireAlphaNum username
    *> checkUsernameLength username

reverseLine :: IO ()
reverseLine = do
  line <- getLine
  print $ reverse line

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
  User <$> validateUsername username <*> validatePassword password

promptWord :: T.Text -> IO T.Text
promptWord wordName =
  T.putStr ("Please enter a " <> wordName <> "\n") *> T.getLine

main :: IO ()
main =
  makeUser
    <$> (Username <$> promptWord "username")
    <*> (Password <$> promptWord "password")
    >>= print

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
    (checkPasswordLength "")
    (Failure $ Error
      "Your password cannot be longer than 19 or shorter than 11 characters"
    )
  eq 2
     (checkPasswordLength "julielovebooks")
     (Success $ Password "julielovebooks")
