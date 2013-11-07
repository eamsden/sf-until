module FFTW

import Prelude.Complex
import DoublePtr
import IO

%include C "fftw3.h"
%lib C "fftw3" 

natToInt : Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

data FftwElement = FftwComplex | FftwReal

fftwElementRepr : FftwElement -> Type
fftwElementRepr FftwComplex = Complex Float 
fftwElementRepr FftwReal = Float

data FftwBuffer : FftwElement -> Nat -> Type where
  fftwBuffer : Ptr -> FftwBuffer f n

fftwBufferPtr : FftwBuffer e n -> Ptr
fftwBufferPtr (fftwBuffer p) = p

data FftwPlan : (n : Nat) -> FftwElement -> FftwElement -> Type where
  realComplexPlan :  FftwBuffer FftwReal (n + n)
                  -> FftwBuffer FftwComplex (S n)
                  -> Ptr
                  -> FftwPlan n FftwReal FftwComplex

FFTW_ESTIMATE : interpFTy FInt
FFTW_ESTIMATE = 64

fftw_malloc : Int -> IO Ptr
fftw_malloc i = mkForeign (FFun "fftw_malloc" [FInt] FPtr) i

fftw_destroy_plan : Ptr -> IO ()
fftw_destroy_plan p = mkForeign (FFun "fftw_destroy_plan" [FPtr] FUnit) p

fftw_free : Ptr -> IO ()
fftw_free p = mkForeign (FFun "fftw_free" [FPtr] FUnit) p

fftw_plan_dft_r2c_1d : Int -> Ptr -> Ptr -> Int -> IO Ptr
fftw_plan_dft_r2c_1d n i o f = mkForeign (FFun "fftw_plan_dft_r2c_1d" [FInt, FPtr, FPtr, FInt] FPtr) n i o f

fftw_execute : Ptr -> IO ()
fftw_execute p = mkForeign (FFun "fftw_execute" [FPtr] FUnit) p

fftwPlanR2C : FftwBuffer FftwReal (n + n) -> FftwBuffer FftwComplex (S n) -> IO (FftwPlan n FftwReal FftwComplex)
fftwPlanR2C {n} bin bout = do
  p <- fftw_plan_dft_r2c_1d (natToInt (n + n)) (fftwBufferPtr bin) (fftwBufferPtr bout) FFTW_ESTIMATE
  return (realComplexPlan bin bout p)

makeFftwBuffer : (f : FftwElement) -> (n : Nat) -> IO (FftwBuffer f n)
makeFftwBuffer FftwReal n = do
  p <- fftw_malloc (natToInt n)
  return (fftwBuffer p)
makeFftwBuffer FftwComplex n = do
  p <- fftw_malloc (2 * natToInt n)
  return (fftwBuffer p)

initRealComplexPlan : (n : Nat) -> IO (FftwPlan n FftwReal FftwComplex)
initRealComplexPlan n = do
  inBuf <- makeFftwBuffer FftwReal (n + n)
  outBuf <- makeFftwBuffer FftwComplex (S n)
  fftwPlanR2C inBuf outBuf

destroyPlan : FftwPlan n i o -> IO ()
destroyPlan (realComplexPlan (fftwBuffer ip) (fftwBuffer op) p) = do
  fftw_destroy_plan p
  fftw_free ip
  fftw_free op

omega : (n : Nat) -> Vect n Nat
omega Z = []
omega (S n) = n :: omega n

indexedVect : Vect n a -> Vect n (Nat, a)
indexedVect {n} v = zip (omega n) v


putDoubleVect' : Ptr -> Vect n Float -> IO (Vect n ())
putDoubleVect' p v = traverse (\(idx, f) => do put_double p (natToInt idx) f ; return ()) (indexedVect v) 

putDoubleVect : Ptr -> Vect n Float -> IO ()
putDoubleVect p v = do
    _ <- putDoubleVect' p v
    return ()

readDoubleVect : Ptr -> IO (Vect n Float)
readDoubleVect {n} p = traverse (\idx => do get_double p (natToInt idx)) (omega n)

readComplexVect : Ptr -> IO (Vect n (Complex Float))
readComplexVect {n} p = traverse (\idx => do x <- get_double p (natToInt idx) ; y <- get_double p (natToInt (S idx)) ; return (x:+y)) (map (\x => x + x) (omega n))

runPlan : (p : FftwPlan n i o) -> Vect (n + n) (fftwElementRepr i) -> IO (Vect (S n) (fftwElementRepr o))
runPlan (realComplexPlan bin bout ptr) vIn = do
  putDoubleVect (fftwBufferPtr bin) vIn
  fftw_execute ptr
  readComplexVect (fftwBufferPtr bout)
  
