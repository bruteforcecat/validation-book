module Main where

import           Data.Char
import           Data.Validation

newtype Password = Password String deriving (Show, Eq)

newtype Error = Error String deriving (Show, Eq)

newtype Username = Username String deriving (Show, Eq)

instance Semigroup Error where
  Error xs <> Error ys = Error (xs <> ys)

checkLength :: Int -> Int -> String -> Validation Error String
checkLength min max x = case (pwLen > max || pwLen < min) of
  True ->
    Failure
      $  Error
      $  "It is <longer than "
      <> show max
      <> " or shorter than "
      <> show min
      <> " characters"
  False -> Success x
  where pwLen = length x

checkPasswordLength :: String -> Validation Error Password
checkPasswordLength = fmap Password . checkLength 11 19

checkUsernameLength :: String -> Validation Error Username
checkUsernameLength = fmap Username . checkLength 3 12


requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs = case (all isAlphaNum xs) of
  False -> Failure
    $ Error "Your password has to be consisted of numeric or alpha character"
  True -> Success xs

cleanWhitespace :: String -> Validation Error String
cleanWhitespace ""       = Failure $ Error "Empty String is not allowed"
cleanWhitespace (x : xs) = case (isSpace x) of
  True  -> cleanWhitespace xs
  False -> Success (x : xs)

validatePassword :: Password -> Validation Error Password
validatePassword (Password password) =
  case (cleanWhitespace password) of
    Failure err -> Failure err
    Success password2 -> requireAlphaNum password2 *> checkPasswordLength password2

validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  cleanWhitespace username *> requireAlphaNum username *> checkUsernameLength username

reverseLine :: IO ()
reverseLine = do
  line <- getLine
  print $ reverse line

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe (Just a) f = f a
bindMaybe Nothing  _ = Nothing

data StringOrValue a = Str String | Val a deriving Show

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

promptWord :: String -> IO String
promptWord wordName =
  putStr ("Please enter a"<> wordName <> "\n") *> getLine

main :: IO ()
main = do
  username <- Username <$> promptWord "username"
  password <- Password <$> promptWord "password"
  print $ (makeUser username password)

printTestResult :: Either String () -> IO ()
printTestResult r = case r of
  Left  err -> putStrLn err
  Right ()  -> putStrLn "All tests passed"

eq :: (Eq a, Show a) => Int -> a -> a -> Either String ()
eq n actual expected = case (actual == expected) of
  True  -> Right ()
  False -> Left
    (unlines
      [ "Test " ++ show n
      , " Expected:  " ++ show expected
      , " But got:   " ++ show actual
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
