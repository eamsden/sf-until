module FFTW

%include C "fftw3.h"

%lib C "fftw3" 

data FftwElement = FftwComplex | FftwReal

data FftwBuffer : FftwElement -> Nat -> Type where
  fftwBuffer : interpFTy FPtr -> FftwBuffer f n

data FftwPlan : FftwElement -> FftwElement -> Type where
  realComplexPlan : (n : Nat) -> FftwBuffer FftwReal (n + n) -> FftwBuffer FftwComplex (S n) -> interpFTyp FPtr -> FftwPlan FftwReal FftwComplex

fftwPlanSize : FftwPlan i o -> Nat
fftwPlanSize (realComplexPlan n _ _ _) = n

FFTW_ESTIMATE : interpFTy FInt
FFTW_ESTIMATE = 64

fftw_alloc : interpFTy FInt -> IO (interpFTy FPtr)
fftw_alloc = mkForeign (FFun "fftw_alloc" [FInt] FPtr)

fftw_plan_dft_r2c_1d : interpFTy FInt -> interpFTy FPtr -> interpFTy FPtr -> interpFTy FInt -> IO (interpFTy FPtr)
fftw_plan_dft_r2c_1d = mkForeign (FFun "fftw_plan_dft_r2c_1d" [FInt, FPtr, FPtr, FInt] FPtr)

fftw_execute_plan : interpFTy FPtr -> IO (interpFTy FUnit)
fftw_execute_plan = mkForeign (FFun "fftw_execute_plan" [FPtr] FUnit)

fftwPlanR2C : FftwBuffer FftwReal (n + n) -> FftwBuffer (S n) -> IO (FftwPlan FftwReal FftwComplex)
fftwPlanR2c {n} bin bout = do
  p <- fftw_plan_dft_r2c_1d (natToInt (n + n)) bin bout FFTW_ESTIMATE
  return (FftwPlan n bin bout p)

natToInt : Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

makeFftwBuffer : (f : FftwElement) -> (n : Nat) -> IO FfftwBuffer f n
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



