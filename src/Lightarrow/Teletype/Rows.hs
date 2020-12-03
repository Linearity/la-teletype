module Lightarrow.Teletype.Rows where

import Control.Monad.Fix
import Data.Bifunctor
import FRP.BearRiver hiding (first)
import Lightarrow
import Lightarrow.Teletype.Printer
import Linear

-- | Parameters for printing and scrolling text in a certain number of rows.
data Config a b m
    = Config {  -- | number of rows
                cRows           :: Int,
                -- | typing activity, given pending text and lines already printed
                cTyping         :: String
                                       -> [String]
                                       -> Mode a b m (String, [String]),
                -- | activity between typing and scrolling, given lines already printed
                cWaiting        :: [String] -> Mode a b m (),
                -- | scrolling activity, given lines already printed
                cScrolling      :: [String] -> Mode a b m (),
                -- | final activity after all lines are printed
                cFinish         :: [String] -> Mode a b m ()    }

{-|
Starting from nothing, print text until some number of lines are filled.
When finished, return both the remaining text and the filled lines.
-}
fill :: Monad m =>
            Config a b m                            -- ^ configuration, incl. number of lines
                -> String                           -- ^ text to be printed
                -> Mode a b m (String, [String])    -- ^ the line-filling activity
fill config text = fillLoop 1 text []
    where   fillLoop _n "" lines   = return ("", lines)
            fillLoop n pending lines
                = do    (pending', lines') <- cTyping config pending lines
                        if n == cRows config
                            then return (pending', lines')
                            else fillLoop (n + 1) pending' lines' 

{-|
Starting from a list of printed lines, print all pending text on
subsequent lines.  Each printed line is followed by a scrolling action,
which frees up a new row by hiding the oldest row.  When finished, return the
last lines visible.
-}
scroll :: Monad m =>
            Config a b m                    -- ^ configuration
                -> String                   -- ^ pending text
                -> [String]                 -- ^ lines already printed
                -> Mode a b m [String]      -- ^ the line-scrolling activity
scroll _config "" lines  = return lines
scroll config pending lines
    = do    cWaiting config lines
            cScrolling config lines
            (pending', lines') <- cTyping config pending (tail lines)
            scroll config pending' lines'

{-|
A typing activity based on 'printer', plus an extra input signal for cues
and an extra output signal for a scrolling offset vector.
-}
typing :: (Additive f, Num a, MonadFix m) =>
                Int                                     -- ^ line width
                    -> String                           -- ^ pending text
                    -> [String]                         -- ^ lines already printed
                    -> Mode     (Time, b)
                                ([String], f a)
                                m
                                (String, [String])      -- ^ the typing activity
typing width pending lines = mapMode wrap (printer width pending lines)
    where wrap sf = arr fst >>> sf >>> arr (first (, zero))

{-|
Wait with a given output and a zero scrolling offset vector until
a given reaction to the cue (second) input signal.
-}
waiting :: (Additive f, Num n, Monad m) =>
                (c -> Event d)                          -- ^ a cue reaction
                    -> b                                -- ^ lines, or any output
                    -> Mode (a, c) (b, f n) m d         -- ^ the waiting activity
waiting react lines = onlyUntil scroll
                        (always (constant (lines, zero)))
    where scroll = arr (\((_interval, cue), _) -> react cue)

{-|
Scroll lines as in Pokémon Red/Blue: offset by a constant half-line-height
during the full scrolling interval.
-}
halfScrolling :: (Monad m, Fractional n) =>
                    DTime                               -- ^ scrolling interval
                        -> n                            -- ^ line height
                        -> b                            -- ^ lines, or any output
                        -> Mode a (b, V2 n) m ()        -- ^ the scrolling activity
halfScrolling interval lineheight lines
    = over interval (constant (lines, V2 0 (-lineheight / 2)))

-- | Scroll lines continuously through one line height during the scrolling interval.
smoothScrolling :: Monad m =>
                    DTime                               -- ^ scrolling interval
                        -> Double                       -- ^ line height
                        -> b                            -- ^ lines, or any output
                        -> Mode a (b, V2 Double) m ()   -- ^ the scrolling activity
smoothScrolling interval lineheight lines
    = over interval (time >>> arr (\t -> (lines, V2 0 (-lineheight * t / interval))))

-- | Output no lines for a certain interval
breathing :: (Additive f, Monad m, Num c) =>
                Time                            -- ^ interval
                    -> Mode a ([b], f c) m ()   -- ^ the breathing activity
breathing interval = over interval (constant ([], zero))