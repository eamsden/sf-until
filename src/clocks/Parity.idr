module Parity

data Parity : Nat -> Type where
  even : Parity (n + n)
  odd  : Parity (S (n + n))

parity : (n : Nat) -> Parity n
parity Z = even {n = Z}
parity (S Z) = odd {n = Z}
parity (S (S n)) with (parity n)
  parity (S (S (j + j)))     | even ?= even {n = (S j)}
  parity (S (S (S (j + j)))) | odd  ?= odd {n = (S j)}

oddToEven : Parity (S (n + n)) -> Parity (S n + S n)
oddToEven {n} odd = even {n = S n}


---------- Proofs ----------

Parity.parity_lemma_2 = proof
  intros
  rewrite sym (plusSuccRightSucc j j)
  refine value


Parity.parity_lemma_1 = proof
  intros
  rewrite sym (plusSuccRightSucc j j)
  refine value


