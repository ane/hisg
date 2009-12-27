module Formatter where

import Types
import System.IO
import Stats
import Misc
import Data.List

writeHeaders :: Handle -> String -> String -> Log -> IO ()
writeHeaders out chan stylesheet logf = do
    let dates = getDates logf
    hPutStrLn out $ "<html>\n<head><title>Statistics for #" ++ chan ++ "</title>"
    hPutStrLn out $ "<link rel=\"stylesheet\" type=\"text/css\" href=\"" ++ stylesheet ++ "\" />"
    hPutStrLn out "</style>\n<body>\n"
    hPutStrLn out $ "<h1>Statistics for #" ++ takeWhile (/= '.') chan ++ "</h1>"
    hPutStrLn out $ "<p>Data from " ++ show (head dates) ++ " &mdash; " ++ show (last dates) ++ "<br/>"
    hPutStrLn out $ "A total of " ++ show (length logf) ++ " lines were spoken during this period.</p>"

writeUsersTable :: Handle -> [User] -> IO ()
writeUsersTable out users = do
    hPutStrLn out "<h2>Top 25 users</h2>"
    hPutStrLn out $ "<table>\n<tr><th>Nickname</th><th>Number of lines</th><th>Number of words</th></tr>" ++ concatMap (\(rank, u) -> "<tr><td><b>" ++ show rank ++ ".</b> " ++ user_nickname u ++ "</td><td>" ++ show (user_line u) ++ "</td><td>" ++ show (user_words u) ++ "</td></tr>") (zip [1..] users) ++ "</table>"

writeMiscStats :: Handle -> Log -> IO ()
writeMiscStats out logf = do
    let kicks = getKicks logf
        kickers = common (map getKicker kicks)
        kickeds = common (map getKicked kicks)
        topkicker = head kickers
        sndkicker = kickers !! 1
        topkicked = head kickeds
        sndkicked = kickeds !! 1

    hPutStrLn out "<h2>Miscellaneous stats</h2>\n<table>"
    if not . null $ topkicker
        then do
            hPutStrLn out $ "<tr><td><b>" ++ head topkicker ++ "</b> acted as the channel judge. He kicked a total of <b>" ++ show (length topkicker) ++ "</b> people!<br/>"
            if not . null $ sndkicker
                then do hPutStrLn out $ "His lieutenant, <b>" ++ head sndkicker ++ "</b>, assisted with <b>" ++ show (length sndkicker) ++ "</b> kicks!</td></tr>"
                else do hPutStrLn out "No one else kicked people in the channel."
            hPutStrLn out $ "<tr><td>Nobody liked <b>" ++ head topkicked ++ "</b>. He got kicked " ++ show (length topkicked) ++ "</b> times!<br/>"
            if not . null $ sndkicked
                then do hPutStrLn out $ "<b>" ++ head sndkicked ++ "</b> came second on the loser chart, getting kicked " ++ show (length sndkicked) ++ "</b> times!<br/></td></tr>"
                else do hPutStrLn out "He was the only to get kicked!"
        else do putStrLn "Nobody kicked anyone during this period."

    hPutStrLn out "</table>"
    where
        getKicked (KickEvent (Kick _ _ kicked _)) = kicked
        getKicker (KickEvent (Kick _ kicker _ _)) = kicker

footer :: String -> String
footer ver = "<p>Generated with <a href=\"http://code.google.com/p/hessu\">Hessu</a> v" ++ ver ++ "</p></body></html>"
