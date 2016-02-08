{-
Lee Pike
<http://www.cs.indiana.edu/~lepike/>

Code adapted from John Van Enk's source, found in his blog post
<http://blog.sw17ch.com/wordpress/?p=111>, about using Atom
<http://hackage.haskell.org/packages/archive/atom/0.1.0/doc/html/Language-Atom-Code.html>
to code the Arduino <http://www.arduino.cc/>.

Compatible with Atom 1.0.  No C files needed.
-}

module Main where

import Language.Atom
import Data.Word

include :: String -> String
include lib = "#include<" ++ lib ++ ">"

blinkHeader :: String
blinkHeader = unlines
  [ include "avr/io.h"
  , include "util/delay.h"
  , include "stdint.h"
  , "void blink(void);"
  ]

driver :: String
driver = unlines
  [ "/* main only needs to continously call blink_atom(). */"
  , "int main()"
  , "  { while(1)"
  , "     { blink();}"
  , "  }"
  ]

-- Override some default values
config :: Config
config = defaults {
    cPreCode  = blinkHeader,
    cPostCode = driver,
    cFuncName = "blink",
--    cFuncName = "",

    -- The (u)intXX_t types are defined in
    -- stdint.h--which we include from blink.h
    cType     = \t -> case t of
                       Bool   -> "uint8_t"
                       Int8   -> "int8_t"
                       Int16  -> "int16_t"
                       Int32  -> "int32_t"
                       Int64  -> "int64_t"
                       Word8  -> "uint8_t"
                       Word16 -> "uint16_t"
                       Word32 -> "uint32_t"
                       Word64 -> "uint64_t"
                       Float  -> "float"
                       Double -> "double"
}

-- Main just has to compile the Atom expression
main :: IO ()
main = do compile "main" config blink
          return ()

-- How many cycles do we want to delay before 
-- we flip the LED?
delayCycles :: Word16
delayCycles = 30000

-- Simple Atom to toggle an LED
blink :: Atom ()
blink = do
    -- Output vars (corresponding to pins)
    dDRD <- word8' "DDRD"

    -- Initialization of external vars.
    initialize <- bool "initialize" True

    pORTD <- word8' "PORTD"

    -- Is the LED currently on? (Assume it starts False/off)
    isOn    <- bool "isOn" False

    -- Does the toggle counter need a reset? (Assume it starts False/no)
    doReset <- bool "doReset" False

    -- Initialize the toggle counter to delayCycles
    toggle  <- word16 "toggle" delayCycles

    -- Initialize the pin
    period 1 $ atom "init" $ do
      cond $ value initialize
      dDRD       <== 255
      initialize <== false

    -- Decrements the toggle counter when it
    -- is greater than 0.
    period 1 $ atom "decrement" $ do
        cond $ value toggle >. 0
        decr toggle

    -- Checks if we need to perform a toggle
    -- reset, and performs it when we need one.
    period 2 $ atom "reset" $ do
        cond $ value doReset
        doReset <== false
        toggle  <== Const delayCycles

    -- Checks if the toggle counter has expired.
    -- Toggles the LED if it has, then requests
    -- a reset.
    period 2 $ atom "flip" $ do
        cond $ value toggle <=. 0
        -- where the magic happens (i.e., signal pins)
        pORTD <== mux (value isOn) 255 0
        isOn <== (not_ $ value isOn)
        doReset <== true
