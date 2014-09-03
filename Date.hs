module Date (getWhen) where

import System.IO
import System.Locale
import Data.List
import Data.Time
import Data.Maybe
import Data.Char
import Control.Applicative

dispatchDay :: [(String, (LocalTime -> LocalTime))]
dispatchDay = [ ("today", id)
               , ("yesterday", (flip daysBefore) 1)
               , ("day before yesterday", (flip daysBefore) 2)
               ]

timeFormats :: [String]
timeFormats = [ "%l %p"
              , "%l:%M %p"
              ]

dayFormats :: [String]
dayFormats = [ "%b %e"
             , "%B %e"
             , "%m/%e"
             , "%m/%e/%y"
             , "%m/%e/%Y"
             ]


lookupPrefix :: String -> [(String, b)] -> Maybe b
lookupPrefix k pairs = case filter (\(x, _) -> x `isInfixOf` k) pairs of
    [] -> Nothing
    v  -> Just $ snd $ head v

daysBefore :: LocalTime -> Integer -> LocalTime
daysBefore (LocalTime d t) x  = LocalTime (addDays (- x) d) t

constructDaysOfWeek :: LocalTime -> [(String, LocalTime)]
constructDaysOfWeek today = map (getWeekDayOfDaysBefore today) [1 .. 7]
    where getWeekDayOfDaysBefore d x = (dayOfWeek, d')
            where
                d' = daysBefore d x
                dayOfWeek = map toLower $ formatTime defaultTimeLocale "%a" d'


splitToDayAndTime :: [String] -> Maybe (String, String)
splitToDayAndTime xs = case break (== "at") xs of
    (_, [])         -> Just (init $ unwords xs, "")
    (_, at:[])      -> Nothing
    (date, at:time) -> Just (unwords date, init $ unwords time)



matchTime :: String -> [String] -> Maybe LocalTime
matchTime t fs = case m of
    [] -> Nothing
    _  -> head m
    where
        m = filter isJust $ map (parseTimeWithFormat t) fs
        parseTimeWithFormat t format = parseTime defaultTimeLocale format t


constructTime :: (String, String) -> LocalTime -> Maybe LocalTime
constructTime ("", "") t = Just t

constructTime (d, "") t = case matchTime d dayFormats of
    Nothing -> case lookup d dispatchDay of
                   Nothing -> lookupPrefix d $ constructDaysOfWeek t
                   Just f  -> Just $ f t
    Just m 	-> Just $ getNewTime m
    where
        getNewTime matched = LocalTime (fromGregorian y m d) (localTimeOfDay t)
            where
                (_, m, d) = toGregorian $ localDay matched
                (y, _, _) = toGregorian $ localDay t

constructTime (d, "noon") t     = constructTime (d, "12:00 pm") t

constructTime (d, "midnight") t = constructTime (d, "12:00 am") t

constructTime (d, t) (LocalTime defDay _) =
    matchTime t timeFormats >>=
    constructTime (d, "") . LocalTime defDay . localTimeOfDay


today :: IO ZonedTime
today = getZonedTime

getWhen :: [String] -> IO (Maybe ZonedTime)
getWhen [] = Just <$> today
getWhen xs = do
    (ZonedTime d zone) <- today
    return $ splitToDayAndTime xs >>= \x ->
         ((flip ZonedTime) zone) <$> (constructTime x d)
