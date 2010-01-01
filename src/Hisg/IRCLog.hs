-- hisg - IRC stats generator.
--
-- Copyright (c) 2009, 2010 Antoine Kalmbach <antoine dot kalmbach at jyu dot fi>
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the author nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- For further details, see LICENSE.
--
--

module Hisg.IRCLog where

import System.IO
import Data.Maybe

import Hisg.Parser
import Hisg.Types

data IRCLog = IRCLog { filename :: String, contents :: [LogEvent] }

parseInput :: String -> Log
parseInput inp = map (fromMaybe (Simple "") . (decode . (++ "\n"))) (lines inp)

loadLog :: String -> IO IRCLog
loadLog fn = do
    infile <- openFile fn ReadMode
    inp <- hGetContents infile
    return $ IRCLog fn (parseInput inp)
