module SFClock

import Prelude.Complex
import Decidable.Equality
import FFTW
import Parity

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
clockStep (EvenlySpaced _) = ()  -- Token to take next step

-- | The type of a sample, given a clock step
total clockSample : {c : Clock} -> clockStep c -> Type -> Type
clockSample {c=Whatever} _ t = t
clockSample {c=EvenlySpaced _} _ t = t

-- | Signal functions
data SF : Clock -> SignalVector -> SignalVector -> Type where
  Pure     : (sampleType i -> sampleType o) -> SF clock i o
  Clocked  : a -> ((s : clockStep clock) -> clockSample s (sampleType i) -> a -> (clockSample s (sampleType o), a)) -> SF clock i o

-- | Sampling a signal function
total sample : SF clock i o -> (s : clockStep clock) -> clockSample s (sampleType i) -> (clockSample s (sampleType o), SF clock i o)
sample {clock=Whatever} (Pure f) step samp  = (f samp, (Pure f))
sample {clock=Whatever} (Clocked acc stepper) step samp = let (out, acc') = stepper step samp acc in (out, Clocked acc' stepper)
sample {clock=EvenlySpaced td} (Pure f) step samp = (f samp, (Pure f))
sample {clock=EvenlySpaced td} (Clocked acc stepper) step samp = let (out, acc') = stepper step samp acc in (out, Clocked acc' stepper)

-- Basic signal functions

-- | Identity signal function
identity : SF clock i i
identity = Pure id

-- | Lift pure function pointwise to signal function
lift : (sampleType a -> sampleType b) -> SF clock a b
lift f = Pure f

-- | Compose two signal functions pointwise
(>>>) : SF clock i m -> SF clock m o -> SF clock i o
(>>>) {clock=Whatever} (Pure f) (Pure g) = Pure (g . f)
(>>>) {clock=Whatever} (Pure f) (Clocked acc stepper) = Clocked acc (\st, sa, a => stepper st (f sa) a)
(>>>) {clock=Whatever} (Clocked acc stepper) (Pure g) = Clocked acc (\st, sa, a => let (sa', a') = stepper st sa a in (g sa', a'))
(>>>) {clock=Whatever} (Clocked acc1 stepper1) (Clocked acc2 stepper2) = Clocked (acc1, acc2) (\st, sa, a => let (a1, a2) = a in let (sa', a1') = stepper1 st sa a1 in let (sa'', a2') = stepper2 st sa' a2 in (sa'', (a1', a2')))
(>>>) {clock=EvenlySpaced _} (Pure f) (Pure g) = Pure (g . f)
(>>>) {clock=EvenlySpaced _} (Pure f) (Clocked acc stepper) = Clocked acc (\st, sa, a => stepper st (f sa) a)
(>>>) {clock=EvenlySpaced _} (Clocked acc stepper) (Pure g) = Clocked acc (\st, sa, a => let (sa', a') = stepper st sa a in (g sa', a'))
(>>>) {clock=EvenlySpaced _} (Clocked acc1 stepper1) (Clocked acc2 stepper2) = Clocked (acc1, acc2) (\st, sa, a => let (a1, a2) = a in let (sa', a1') = stepper1 st sa a1 in let (sa'', a2') = stepper2 st sa' a2 in (sa'', (a1', a2')))

takeIndeterminate : Nat -> Vect n a -> (p ** Vect p a)
takeIndeterminate _ [] = (Z ** [])
takeIndeterminate Z _  = (Z ** [])
takeIndeterminate (S n) (x :: xs) = let (p ** ys) = takeIndeterminate n xs in (S p ** x :: ys)

windowStart : (p ** Vect p a)
windowStart = (Z ** [])

reverseDepVect : (p ** Vect p a) -> (p ** Vect p a)
reverseDepVect (p ** v) = (p ** reverse v)

reverseDepVectPair : (p ** Vect p a) -> ((p ** Vect p a), (p ** Vect p a))
reverseDepVectPair dp = (reverseDepVect dp, dp)

window : {td : Float} -> Nat -> SF (EvenlySpaced td) (B a) (B (p ** Vect p a))
window {a} n = Clocked (windowStart {a}) (\(), sa, (_ ** oldWindow) => let newWindow = takeIndeterminate n (sa :: oldWindow) in reverseDepVectPair newWindow)

oddVectorToEvenVector : a -> Vect (S (j + j)) a -> Vect (S j + S j) a
oddVectorToEvenVector a v ?= a :: v

evenVector : a -> (k ** Vect k a) -> (m ** Vect (m + m) a)
evenVector filler (k ** v) with (parity k)
  evenVector filler ((j + j) ** v)     | even = (j ** v)
  evenVector filler ((S (j + j)) ** v) | odd  = (S j ** oddVectorToEvenVector filler v)

fftwStart : Maybe (s ** FftwPlan s FftwReal FftwComplex)
fftwStart = Nothing

returnIO : a -> IO a
returnIO x = return x

tryOldPlan : (s : Nat) -> (t ** FftwPlan t FftwReal FftwComplex) -> IO (FftwPlan s FftwReal FftwComplex)
tryOldPlan s (t ** oldPlan) with (decEq s t)
  tryOldPlan t (t ** oldPlan) | (Yes sEqT) ?= returnIO oldPlan
  tryOldPlan s _              | (No _)     = initRealComplexPlan s

fftw : {td : Float} -> SF (EvenlySpaced td) (B (p ** Vect p Float)) (B (q ** Vect q (Complex Float)))
fftw = Clocked fftwStart (\(), winDP, mPlan => let (fftSize ** fftIn) = evenVector 0 winDP in let outSize = S fftSize in 
                                                   unsafePerformIO $ do
                                                     plan <- case mPlan of
                                                                  Nothing => initRealComplexPlan fftSize
                                                                  Just op => tryOldPlan fftSize op
                                                     out <- runPlan plan fftIn
                                                     return ((outSize ** out), Just (fftSize ** plan)))
                                                     

testFFTW : SF (EvenlySpaced td) (B Float) (B (q ** Vect q (Complex Float)))
testFFTW = (>>>) (window 256) fftw

---------- Proofs ----------

SFClock.oddVectorToEvenVector_lemma_1 = proof
  intros
  rewrite (plusSuccRightSucc j j)
  refine value


