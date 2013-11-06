module Pulse

%include C "idris-pa.h"
%link C "idris-pa.o"
%lib C "pulse"
%lib C "pulse-simple"

pa_start_in : String -> String -> IO Ptr
pa_start_in an sn = mkForeign (FFun "pa_start_in" [FString, FString] FPtr) an sn

pa_start_out : String -> String -> IO Ptr
pa_start_out an sn = mkForeign (FFun "pa_start_out" [FString, FString] FPtr) an sn

pa_stop : Ptr -> IO ()
pa_stop pa = mkForeign (FFun "pa_stop" [FPtr] FUnit) pa

pa_get : Ptr -> IO Float
pa_get pa = mkForeign (FFun "pa_get" [FPtr] FFloat) pa

pa_put : Ptr -> Float -> IO ()
pa_put pa x = mkForeign (FFun "pa_put" [FPtr, FFloat] FUnit) pa x

data Dir = In | Out

data PA : Dir -> Type where
  PAIn : Ptr -> PA In
  PAOut : Ptr -> PA Out

paStart : (d : Dir) -> String -> String -> IO (PA d)
paStart In an sn = do
  ptr <- pa_start_in an sn
  return $ PAIn ptr
paStart Out an sn = do
  ptr <- pa_start_out an sn
  return $ PAOut ptr

paStop : PA d -> IO ()
paStop (PAIn p) = pa_stop p
paStop (PAOut p) = pa_stop p

paGet : PA In -> IO Float
paGet (PAIn ptr) = pa_get ptr

paPut : PA Out -> Float -> IO ()
paPut (PAOut ptr) x = pa_put ptr x


