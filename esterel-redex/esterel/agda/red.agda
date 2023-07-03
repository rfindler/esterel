module _ where
open import nat
open import eq
open import bool
open import list
open import e
open import ec
open import env

data S⊥ : Set where
  S⊥Sigs : list ℕ -> S⊥
  S⊥⊥ : S⊥

data red : ∀ { sd T } ->
  E sd (TD 0) T -> Env -> S⊥ ->
  E sd (TD 0) T -> Env -> S⊥ ->
  Set where

  ift : ∀ { env T_outer sd td T EC S⊥ } -> 
    (e_left e_right : E (SD (length env)) (TD 0) T_outer) ->
    (e1 e2 : E (SD (length env + sd)) td T) ->
    Decomp e_left EC (if (B tt) e1 e2) ->
    Decomp e_right EC e1 ->
    red e_left sd S⊥ e_right sd S⊥

  seq : ∀ { env td_outer T_outer sd td T T_inner EC S⊥ } -> 
    (e_left e_right : E (length env) (TD 0) T_outer) ->
    (v1 : E (length env + sd) td T) ->
    (e2 : E sd td T_inner) ->
    V v1 ->
    Decomp e_left EC (seq v1 e2) ->
    Decomp e_right EC e2 ->
    red e_left sd S⊥ e_right sd S⊥
