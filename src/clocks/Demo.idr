module Main

import SFClock
import Pulse
import Prelude.Complex

magnitudes : Vect n (Complex Float) -> Vect n Float
magnitudes v = map magnitude v

thresholds : (Ord a) => a -> Vect n a -> Vect n Bool
thresholds x v = map (>= x) v

showThresholds : Vect n Bool -> String
showThresholds v = foldr (\b, s => if b then strCons '*' s else strCons ' ' s) "" v

fft16 : {td : Float} -> SF (EvenlySpaced td) (B Float) (B (n ** Vect n (Complex Float)))
fft16 = window 16 >>> fftw


magnitudesSF : {td : Float} -> SF (EvenlySpaced td) (B Float) (B String)
magnitudesSF = fft16 >>> lift (\(_ ** v) => show (magnitudes v))

demoLoop : {td : Float} -> PA In -> SF (EvenlySpaced td) (B Float) (B String) -> IO ()
demoLoop p sf = do
  x <- paGet p
  let (out, sf') = sample sf () x
  putStrLn out
  demoLoop p sf'

main : IO ()
main = do
  p <- paStart In "FFT Demo" "FFT Demo"
  demoLoop {td = 1/44100} p magnitudesSF

