module User where

import Data.List
import Types

instance Ord User where
  (User n1 w1 l1) <= (User n2 w2 l2) = l1 <= l2
  (User n1 w1 l1) > (User n2 w2 l2) = l1 > l2

instance Eq User where
  (User n1 w1 l1) == (User n2 w2 l2) = n1 == n2 && w1 == w2 && l1 == l2

matchNick nick (Message ts nick' cont) = nick == nick'
matchNick _ _ = False

getNicks :: Logfile -> [String]
getNicks logfile = nub $ map getNick (filter isMessage logfile)
    where
        getNick (Message ts nick cont) = nick
        getNick _ = ""

isMessage (Message _ _ _) = True
isMessage _ = False

getUserLines :: String -> Logfile -> [String]
getUserLines nick logf = map content (filter match logf)
    where
        match line = isMessage line && matchNick nick line

getUserWords :: [String] -> [String]
getUserWords = concatMap words

getUserStats :: Logfile -> [User]
getUserStats logf = map buildU (getNicks logf)
    where
        buildU nick = let ls = getUserLines nick logf in User nick (length (getUserWords (nub ls))) (length ls)

