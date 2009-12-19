module Types where

import Data.List
import Data.Char
import Text.Printf

-- Basic types
data Timestamp = Timestamp { ts_hour :: Int, ts_minute :: Int }
data Date = Date { date_day  :: String, date_month :: String, date_year :: String }
data User = User { user_nickname :: String, user_words :: Int, user_line :: Int }
data Event = Event { event_ts :: Timestamp, event_type :: EventType, event_user :: String, event_host :: String, event_param :: String }

data Logline =
  Message { timestamp :: Timestamp, nickname  :: String, content :: String }
  | Notification String
  | DateChange Date
  | LogEvent Event
  | Simple String

-- Type aliases

type Logfile = [Logline]
type EventType = String

instance Show Timestamp where
	show (Timestamp h m) = printf "%02d:%02d" h m

instance Show Logline where
  show (Message ts nick content) = show ts ++ " " ++ "<" ++ nick ++ ">" ++ " " ++ content
  show (Notification cont) = cont
  show (DateChange (Date d m y)) = intercalate " " [d, m, y]
  show (LogEvent ev) = show ev
  show (Simple str) = str

instance Show Event where
  show (Event ts evtype evuser host param) = map (toUpper) (show ts) ++ " " ++ show evtype ++ ": " ++ evuser ++ " (" ++ host ++ ")" ++ " -> " ++ param
