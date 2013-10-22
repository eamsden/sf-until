module FFTW

import DoublePtr
import IO

%include C "fftw3.h"
%lib C "fftw3" 

natToInt : Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

data FftwElement = FftwComplex | FftwReal

fftwElementRepr : FftwElement -> Type
fftwElementRepr FftwComplex = (Float, Float)
fftwElementRepr FftwReal = Float

data FftwBuffer : FftwElement -> Nat -> Type where
  fftwBuffer : Ptr -> FftwBuffer f n

fftwBufferPtr : FftwBuffer e n -> Ptr
fftwBufferPtr (fftwBuffer p) = p

data FftwPlan : FftwElement -> FftwElement -> Type where
  realComplexPlan :  (n : Nat) 
                  -> FftwBuffer FftwReal (n + n)
                  -> FftwBuffer FftwComplex (S n)
                  -> Ptr
                  -> FftwPlan FftwReal FftwComplex

fftwPlanInSize : FftwPlan i o -> Nat
fftwPlanInSize (realComplexPlan n _ _ _) = n + n

fftwPlanOutSize : FftwPlan i o -> Nat
fftwPlanOutSize (realComplexPlan n _ _ _) = S n

FFTW_ESTIMATE : interpFTy FInt
FFTW_ESTIMATE = 64

fftw_alloc : Int -> IO Ptr
fftw_alloc i = mkForeign (FFun "fftw_alloc" [FInt] FPtr) i

fftw_plan_dft_r2c_1d : Int -> Ptr -> Ptr -> Int -> IO Ptr
fftw_plan_dft_r2c_1d n i o f = mkForeign (FFun "fftw_plan_dft_r2c_1d" [FInt, FPtr, FPtr, FInt] FPtr) n i o f

fftw_execute_plan : Ptr -> IO ()
fftw_execute_plan p = mkForeign (FFun "fftw_execute_plan" [FPtr] FUnit) p

fftwPlanR2C : FftwBuffer FftwReal (n + n) -> FftwBuffer FftwComplex (S n) -> IO (FftwPlan FftwReal FftwComplex)
fftwPlanR2C {n} bin bout = do
  p <- fftw_plan_dft_r2c_1d (natToInt (n + n)) (fftwBufferPtr bin) (fftwBufferPtr bout) FFTW_ESTIMATE
  return (realComplexPlan n bin bout p)

makeFftwBuffer : (f : FftwElement) -> (n : Nat) -> IO (FftwBuffer f n)
makeFftwBuffer FftwReal n = do
  p <- fftw_alloc (natToInt n)
  return (fftwBuffer p)
makeFftwBuffer FftwComplex n = do
  p <- fftw_alloc (2 * natToInt n)
  return (fftwBuffer p)

initRealComplexPlan : Nat -> IO (FftwPlan FftwReal FftwComplex)
initRealComplexPlan n = do
  inBuf <- makeFftwBuffer FftwReal (n + n)
  outBuf <- makeFftwBuffer FftwComplex (S n)
  fftwPlanR2C inBuf outBuf

omega : (n : Nat) -> Vect n Nat
omega Z = []
omega (S n) = n :: omega n

indexedVect : Vect n a -> Vect n (Nat, a)
indexedVect {n} v = zip (omega n) v



putDoubleVect' : Ptr -> Vect n Float -> IO (Vect n ())
putDoubleVect' p v = traverse (\(idx, f) => do put_double p (natToInt idx) f ; return ()) (indexedVect v) 

putDoubleVect : Ptr -> Vect n Float -> IO ()
putDoubleVect p v = do
    _ <- putDoubleVect p v
    return ()

readDoubleVect : Ptr -> IO (Vect n Float)
readDoubleVect {n} p = traverse (\idx => do get_double p (natToInt idx)) (omega n)

readComplexVect : Ptr -> IO (Vect n (Float, Float))
readComplexVect {n} p = traverse (\idx => do x <- get_double p (natToInt idx) ; y <- get_double p (natToInt (S idx)) ; return (x, y)) (map (\x => x + x) (omega n))

runPlan : (p : FftwPlan i o) -> Vect (fftwPlanInSize p) (fftwElementRepr i) -> IO (Vect (fftwPlanOutSize p) (fftwElementRepr o))
runPlan (realComplexPlan n bin bout ptr) vIn = do
  putDoubleVect (fftwBufferPtr bin) vIn
  fftw_execute_plan ptr
  readComplexVect (fftwBufferPtr bout)
  
