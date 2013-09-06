-- | Combining ideas from Jeffrey's LTL Types FRP and Jeltsch's "Temporal Logic
--   with 'Until', Functional Reactive Programming with Processes, and Concrete
--   Process Categories."
module until

-- | Reactive types describe the input and output of signal functions
data RType : Type where
  finite   : Type -> RType    -> RType
  infinite : Type -> RType    -> RType
  once     : Type             -> RType
  plus     : RType -> RType   -> RType
  times    : RType -> RType   -> RType
  mu       : (RType -> RType) -> RType

-- | An example which guarantees an infinite stream of unit events, each arriving in finite time after the next, punctuating unit "signals"
exampleEventStream : RType
exampleEventStream = mu (\r => finite () (times (once ()) r))

-- | The type of a signal function
(~>) : RType -> RType -> Type
_ ~> _ = _|_

