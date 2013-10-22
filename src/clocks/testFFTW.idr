module Main

import FFTW

n : Nat
n = 6 

test : Vect (n + n) Float
test = 0.0 :: 0.1 :: 0.2 :: 0.3 :: 0.4 :: 0.5 :: 0.6 :: 0.7 :: 0.8 :: 0.9 :: 1.0 :: 0.9 :: []

main : IO ()
main = do
  p <- initRealComplexPlan n
  out <- runPlan p test
  putStrLn (show out)

