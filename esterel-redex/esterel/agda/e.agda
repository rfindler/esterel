module _ where
open import nat
open import eq
open import bool
open import list
open import env

data T : Set where
  B : T
  N : T
  Void : T
  S : T

data op : Set where
  plus : op
  le : op
  eq : op

data SigDepth : Set where
  SD : ℕ -> SigDepth

data TrapDepth : Set where
  TD : ℕ -> TrapDepth

data _∈_ : ℕ -> ℕ -> Set where
 here : ∀ {n} -> 0 ∈ n
 there : ∀ {x n} -> x ∈ n -> x ∈ (suc n)

rng_op : op -> T
rng_op plus = N
rng_op le = B
rng_op eq = B

data E : SigDepth -> TrapDepth -> T -> Set where
 ! : ∀ { sd td } -> E sd td S -> E sd td Void
 S : ∀ { sd td n } -> n ∈ sd -> E (SD sd) td S 
 _⊃_ : ∀ { sd td n T } -> E (SD sd) td T -> n ∈ sd -> E (SD sd) td T
 seq : ∀ { sd td T1 T2 } -> E sd td T1 -> E sd td T2 -> E sd td T2
 _* : ∀ { sd td T } -> E sd td T -> E sd td T
 par : ∀ { sd td T1 T2 } -> E sd td T1 -> E sd td T2 -> E sd td Void
 nothing : ∀ { sd td } -> E sd td Void
 pause : ∀ { sd td } -> E sd td Void
 trap : ∀ { sd td T } -> E sd (TD (suc td)) T -> E sd (TD td) T
 exit : ∀ { sd td T } -> (td' : ℕ) -> td' ∈ td -> E sd (TD td) T
 sig : ∀ { sd td T } -> E (SD (suc sd)) td T -> E (SD sd) td T
 if : ∀ { sd td T } -> E sd td B -> E sd td T -> E sd td T -> E sd td T
 B : ∀ { sd td } -> 𝔹 -> E sd td B
 N : ∀ { sd td } -> ℕ -> E sd td N
 op_e : ∀ { sd td } -> (op : op) -> 
  E sd td N ->
  E sd td N ->
  E sd td (rng_op op)

data V : ∀ { sd td T } -> E sd td T -> Set where
  nothingV : ∀ { sd td } -> V (nothing {sd} {td})
  BV : ∀ { sd td } (b : 𝔹) -> V (B {sd} {td} b)
  NV : ∀ { sd td } (n : ℕ) -> V (N {sd} {td} n)
