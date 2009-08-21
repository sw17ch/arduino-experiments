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
main = compile "blink_atom" cfg blink >> return ()

-- How many milliseconds before we flip the LED?
delayCycles :: Word16
delayCycles = 1000

-- Simple Atom to toggle an LED
blink :: Atom ()
blink = do
    -- Is the LED currently on? (Assume it starts False/off)
    isOn    <- bool "isOn" False

    -- Does the toggle counter need a reset? (Assume it starts False/no)
    doReset <- bool "doReset" False

    -- Initialize the toggle counter to delayCycles
    toggle  <- word16 "toggle" delayCycles

    -- Decrements the toggle counter when it
    -- is greater than 0.
    atom "decrement" $ do
        cond $ value toggle >. 0
        toggle <== value toggle - 1

    -- Checks if we need to perform a toggle
    -- reset, and performs it when we need one.
    atom "reset" $ do
        cond $ value doReset
        doReset <== Const False
        toggle  <== Const delayCycles
    
    -- Checks if the toggle counter has expired.
    -- Toggles the LED if it has, then requests
    -- a reset.
    atom "flip" $ do
        cond $ value toggle <=. 0
        setLED isOn
        isOn <== (not_ $ value isOn)
        doReset <== Const True

-- An action (basically raw C code) to set the value
-- of the LED. setLED() is defined in blink.c.
setLED :: V Bool -> Atom ()
setLED v = action (\[x] -> "setLED(" ++ x ++ ")") [v']
    where v' = ue . value $ v
