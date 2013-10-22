module DoublePtr

import IO

%link C "double_ptr.o"
%include C "double_ptr.h"

put_double : Ptr -> Int -> Float -> IO ()
put_double p o d = mkForeign (FFun "put_double" [FPtr, FInt, FFloat] FUnit) p o d

get_double : Ptr -> Int -> IO Float
get_double p o = mkForeign (FFun "get_double" [FPtr, FInt] FFloat) p o


