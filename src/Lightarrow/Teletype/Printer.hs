module Lightarrow.Teletype.Printer where

import Control.Monad.Fix
import Data.Bifunctor
import FRP.BearRiver hiding (first)
import Lightarrow

{-|
Print a new line of some pending text following some lines already printed.
After printing each glyph, wait an interval given by the input signal.
When finished, return both the remaining text and the filled lines.
-}
printer :: MonadFix m =>
            Int                                     -- ^ line width
                -> String                           -- ^ pending text
                -> [String]                         -- ^ lines already printed
                -> Mode     Time
                            [String]
                            m
                            (String, [String])      -- ^ the line-printing activity
printer width pending ts = lastOut [""]
                                (mapMode (>>> arr addLine)
                                    (printerLine width pending))
    where addLine = first (\t -> ts ++ [t])

{-|
Given some text, print out the first line no longer than a certain width.
After printing each glyph, wait an interval given by the input signal.
When finished, return the remaining text.
-}
printerLine :: Monad m =>
                Int                                 -- ^ line width
                    -> String                       -- ^ text
                    -> Mode Time String m String    -- ^ the line-printing activity
printerLine width fullText = nextWord (words fullText) width []

{-|
Given some list of words, type the next word on one line, and the next,
until the next word would make the line exceed a given width.
After printing each glyph, wait an interval given by the input signal.
When finished, return the remaining words concatenated together.
-}
nextWord :: Monad m =>
                [String]                            -- ^ words
                    -> Int                          -- ^ line width
                    -> String                       -- ^ the line so far
                    -> Mode Time String m String    -- ^ the line-printing activity
nextWord [] _ _             = return []
nextWord (w:ws) room prev
    | length w > room       = return (unwords (w:ws))
    | null ws               = do    text <- nextGlyphLast w prev
                                    nextWord [] 0 text
    | otherwise             = do    text <- nextGlyph w prev
                                    nextWord ws (room - length w - 1) text

-- | Print the last word in a line, one glyph at a time.
nextGlyphLast :: Monad m =>
                    [a]                             -- ^ word
                        -> [a]                      -- ^ the line so far
                        -> Mode Time [a] m [a]      -- ^ the word-printing activity
nextGlyphLast [] prev       = pause prev
nextGlyphLast (g:gs) prev   = do    text <- pause (prev ++ [g])
                                    nextGlyphLast gs text

-- | Print the next word in a line, one glyph at a time.
nextGlyph :: Monad m =>
                    String                              -- ^ word
                        -> String                       -- ^ the line so far
                        -> Mode Time String m String    -- ^ the word-printing activity
nextGlyph [] prev           = pause (prev ++ " ")
nextGlyph (g:gs) prev       = do    text <- pause (prev ++ [g])
                                    nextGlyph gs text

{-|
Between printing glyphs, wait with constant output for a interval given by
the input signal.
-}
pause :: Monad m => b -> Mode Time b m b
pause text                  = onlyUntil (arr fst >>> afterInput text)
                                (always (constant text))