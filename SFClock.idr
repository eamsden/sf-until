module SFClock

-- | Protocol descriptors: Behaviors and their products
data SignalVector : Type where
  B   : Type -> SignalVector
  (*) : SignalVector -> SignalVector -> SignalVector

-- | Extract the representation type for a sample of a signal vector
total sampleType : SignalVector -> Type
sampleType (B t) = t
sampleType (l * r) = (sampleType l, sampleType r)

-- | Clock descriptors
data Clock : Type where
  Whatever : Clock
  EvenlySpaced : Float -> Clock

-- | Types of steps of a clock
total clockStep : Clock -> Type
clockStep Whatever = Float       -- Seconds to next sample
clockStep (EvenlySpaced _) = Nat -- Number of sample steps to take (can we exclude 0 in the type?)

-- | The type of a sample, given a clock step
total clockSample : {c : Clock} -> clockStep c -> Type -> Type
clockSample {c=Whatever} _ t = t
clockSample {c=EvenlySpaced _} n t = Vect n t

-- | Signal functions
data SF : Clock -> SignalVector -> SignalVector -> Type where
  Pure     : (sampleType i -> sampleType o) -> SF clock i o
  Stateful : a -> (Float -> sampleType i -> a -> (sampleType o, a)) -> SF clock i o
  Clocked  : a -> ((s : clockStep clock) -> clockSample s (sampleType i) -> (clockSample s (sampleType o), a)) -> SF clock i o

-- | Sampling a signal function
sample : SF clock i o -> (s : clockStep clock) -> clockSample s (sampleType i) -> (clockSample s (sampleType o), SF clock i o)
sample {clock=Whatever} (Pure f) step samp  = (f samp, (Pure f))
sample {clock=Whatever} (Stateful acc stepper) step samp = let (out, acc') = stepper step samp acc in (out, Stateful acc' stepper)
sample {clock=Whatever} sf step samp        = ?sampleWhatever
sample {clock=EvenlySpaced td} sf step samp = ?sampleEvenlySpaced
