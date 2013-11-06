module Main

import Pulse

pulsePrint : PA In -> IO ()
pulsePrint p = do 
  x <- paGet p
  print x
  pulsePrint p

sampleInverse : Float
sampleInverse = 1 / 44100

freq : Float
freq = 260

mult : Float
mult = sampleInverse * 2 * pi * freq

duration : Int
duration = 10 * 44100

pulseBeep : PA Out -> Int -> IO ()
pulseBeep p i = do
  if i < duration
     then do
       paPut p $ sin (cast i * mult)
       pulseBeep p (i + 1)
     else return ()

main : IO ()
main = do
  p <- paStart Out "PulseTest" "PT_IN"
  pulseBeep p 0

