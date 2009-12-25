module Formatter (
    usersToHTML
    ) where

import Types

usersToHTML :: [User] -> String -> IO ()
usersToHTML users chan = do
    putStrLn "<html><head><style type=\"text/css\"> body { margin: 0px auto; text-align: center; font: 12px Arial, Helvetica, sans-serif; } table { margin: 0px auto; } td { padding: 5px; border: 1px solid #ccc; }</style></head><body>"
    putStrLn $ "<h1>Statistics for #" ++ chan ++ "</h1>"
    putStrLn "<table>"
    putStrLn "<tr><th>Nick</th><th>Lines</th><th>Words</th></tr>"
    mapM_ (\u -> do putStrLn $ "<tr><td>" ++ user_nickname u ++ "</td><td>" ++ show (user_line u) ++ "</td><td>" ++ show (user_words u) ++ "</td></tr>"
        ) users
    putStrLn "</table></body></html>"
