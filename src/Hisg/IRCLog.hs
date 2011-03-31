-- hisg - IRC stats generator.
--
-- Copyright (c) 2009-2011 Antoine Kalmbach <antoine dot kalmbach at jyu dot fi>
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

module Hisg.IRCLog where

import System.IO
import Data.Maybe
import Control.Parallel.Strategies (rwhnf)
import Control.DeepSeq
import Hisg.LineChunks
import Hisg.Stats

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

--data IRCLog = IRCLog { filename :: String, contents :: [[LogEvent]] }
data IRCLog = IRCLog { filename :: String, userScores :: StatsMap}

instance NFData S.ByteString where
    rnf _ = ()    -- not built into Control.Parallel.Strategies

loadLog fn = do
    userStats <- chunkedReadWith calcUserStats fn
    return $ IRCLog fn userStats

