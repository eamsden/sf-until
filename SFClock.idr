module SFClock

class Clock clock where
  stepType : clock -> Type
  step : (c : clock) -> stepType c -> Double
  
class (Clock clock) => EvenlySpaced clock where
  everyStep : clock -> Double
  everyStepConsistent : (c : clock) -> (s : stepType c) -> (step c s = everyStep c)

data DoubleClock : Type where
  doubleClock : DoubleClock

instance Clock DoubleClock where
  stepType doubleClock = Double
  step doubleClock td = td

data ConstantClock : Double -> Type where
  constantClock : (td : Double) -> ConstantClock td

instance Clock (ConstantClock td) where
  stepType (constantClock _) = ()
  step (constantClock td) () = td

instance EvenlySpaced (ConstantClock td) where
  everyStep (constantClock td) = td
  everyStepConsistent (constantClock td) () = refl

