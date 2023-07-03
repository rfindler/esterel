module _ where
open import nat
open import eq
open import bool
open import list

data SV : Set where
  tt : SV
  ff : SV
  ?? : SV

Env : Set
Env = list SV

