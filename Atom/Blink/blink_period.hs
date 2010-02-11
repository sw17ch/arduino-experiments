module Main where

import Language.Atom
import Data.Word

-- Override some default values
cfg = defaults {
    -- This pulls some stuff in we use below.
    cPreCode = "#include \"blink.h\"",

    -- The (u)intXX_t types are defined in
    -- stdint.h--which we include from blink.h
    cType = c99Types
}

-- Main just has to compile the Atom expression
main :: IO ()
main = compile "blink_atom" cfg blink

-- Simple Atom to toggle an LED
blink :: Atom ()
blink = do
    -- Is the LED currently on? (Assume it starts False/off)
    isOn    <- bool "isOn" False

    -- Checks if the toggle counter has expired.
    -- Toggles the LED if it has, then requests
    -- a reset.
    period 5000 $ atom "flip" $ do
        setLED isOn
        isOn <== (not_ $ value isOn)

-- An action (basically raw C code) to set the value
-- of the LED. setLED() is defined in blink.c.
setLED :: V Bool -> Atom ()
setLED v = action (\[x] -> "setLED(" ++ x ++ ")") [v']
    where v' = ue . value $ v
