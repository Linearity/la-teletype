module Main where

import Control.Monad.Identity
import Data.Binary
import FRP.BearRiver hiding (first)
import Lightarrow
import Optics
import Test.Hspec

import Lightarrow.Teletype
import Linear

main :: IO ()
main = do   expected1   <- decodeFile "test/expected/teletypeLine-test1-expected"
            hspec (specPrinterLine expected1)
            expected2   <- decodeFile "test/expected/teletype-test1-expected"
            hspec (specFill expected2)

specFill :: [(DTime, [String])] -> Spec
specFill expected
        = describe "fill"
            (do     it "types \"Computers are awful\" on two 9-character lines"
                        (result `shouldBe` map (_2 %~ (, V2 0 0)) expected))
    where   result      = zip (scanl (+) 0 dts) (take 600 (runIdentity embedding))
            embedding   = embedSF sf (zip dts (repeat ()))
            sf          = driver >>> typer
            dts         = repeat 0.016
            driver      = constant (0.5, NoEvent)
            typer       = runMode tt (const (constant ([""], zero)))
            tt          = fill config "Computers are awful"
            config      = Config {  cRows       = 2,
                                    cTyping     = typing 9,
                                    cWaiting    = waiting (\_ _ -> NoEvent),
                                    cScrolling  = const (return ()),
                                    cFinish     = const (return ())     }

specPrinterLine :: [(DTime, String)] -> Spec
specPrinterLine expected
        = describe "printerLine"
            (it "types \"zygomorphic\" on one 11-character line"
                (result `shouldBe` expected))
    where   result      = zip (scanl (+) 0 dts) (take 383 (runIdentity embedding))
            embedding   = embedSF sf (zip dts (repeat ()))
            sf          = driver >>> typer
            dts         = repeat 0.016
            driver      = constant 0.5
            typer       = runMode tt (const (constant ""))
            tt          = printerLine 11 "zygomorphic"