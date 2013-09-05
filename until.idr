-- | Combining ideas from Jeffrey's LTL Types FRP and Jeltsch's "Temporal Logic
--   with 'Until', Functional Reactive Programming with Processes, and Concrete
--   Process Categories."
module until

-- | Reactive types describe the input and output of signal functions
data RType : Type where
  finite   : Type -> RType  -> RType
  infinite : Type -> RType  -> RType
  once     : Type           -> RType
  plus     : RType -> RType -> RType
  times    : RType -> RType -> RType


