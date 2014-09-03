module Entry
( parseArgs
, Entry
) where

import Date
import System.IO
import Data.List
import Data.Time
import Control.Applicative



endPunctuations :: [String]
endPunctuations = [".", "!", "?"]



data Entry = Entry { timestamp :: ZonedTime
                   , title :: String
                   , body :: String
                   } deriving (Show, Read)



lastSentenceWord :: String -> Bool
lastSentenceWord word = (`isSuffixOf` word) `any` endPunctuations



endsWithColon :: String -> Bool
endsWithColon = isSuffixOf ":"



splitAfter :: (String -> Bool) -> [String] -> ([String], [String])
splitAfter pred xs = case splitted of
    (_, [])                   -> splitted
    (first, (match : second)) -> (first ++ [match], second)
    where splitted = break pred xs



splitAfterColon :: [String] -> ([String], [String])
splitAfterColon xs = case splitted of
    (_, []) -> ([], xs)
    _       -> splitted
    where splitted = splitAfter endsWithColon xs



splitAfterSentenceEnd :: [String] -> ([String], [String])
splitAfterSentenceEnd = splitAfter lastSentenceWord



parseArgs :: [String] -> IO (Maybe Entry)
parseArgs args = do
    let (date, text) = splitAfterColon $ (words . unwords) args
    let (title, body) = splitAfterSentenceEnd text
    timestamp <- getWhen date
    return $ fmap (\t -> Entry t (unwords title) (unwords body)) timestamp
