module Main where

import Language.Atom

cfg :: Config
cfg = defaults {
    cPreCode = unlines ["#include \"pulse.h\""],
    cType    = c99Types
}

main :: IO ()
main = compile "pulse_atom" cfg pulse >> return ()

pulse :: Atom ()
pulse = do
    isOn    <- bool "isOn"  False
    doDim   <- bool "doDim" False
    toggle  <- word8 "toggle"  minTimeOn -- We want to toggle every time...
    dimmer  <- word16 "dimmer" dimmerTime
    timeOn  <- word8 "timeOn"  maxTimeOn
    timeOff <- word8 "timeOff" (maxTimeOn - minTimeOn)
    
    let toggleDone  = (0 >=. value toggle)
        dimmerDone  = (0 >=. value dimmer)

    -- Mange the on/off events.
    atom "set" $ do
        cond $ toggleDone
        toggle <== mux (value isOn) (value timeOn) (value timeOff)
        setLED isOn
        isOn <== not_ (value isOn)

    -- Manage whether we're counting up or down.
    atom "flip" $ do
        let t = mux (value timeOn <=. minTimeOnC) (Const False) (value doDim)
            f = mux (value timeOn >=. maxTimeOnC) (Const True)  (value doDim)

        cond $ dimmerDone
        doDim <== mux (value doDim) t f

    -- Control the pulse timing
    atom "pulse" $ do
        cond $ dimmerDone
        timeOn  <== mux (value doDim) (value timeOn  - 1) (value timeOn  + 1)
        timeOff <== mux (value doDim) (value timeOff + 1) (value timeOff - 1)
        dimmer  <== Const dimmerTime
        
    -- Decrement dimmer if it's not done
    atom "decrDimmer" $ do
        cond $ not_ dimmerDone
        dimmer <== value dimmer - 1

    -- Decrement toggle if it's not done
    atom "decrToggle" $ do
        cond $ not_ toggleDone
        toggle <== value toggle - 1

  where minTimeOn    = 10
        minTimeOnC   = Const minTimeOn
        maxTimeOn    = 100
        maxTimeOnC   = Const maxTimeOn
        dimmerTime   = 100

-- An action (basically raw C code) to set the value
-- of the LED. setLED() is defined in blink.c.
setLED :: V Bool -> Atom ()
setLED v = action (\[x] -> "setLED(" ++ x ++ ")") [v']
    where v' = ue . value $ v
