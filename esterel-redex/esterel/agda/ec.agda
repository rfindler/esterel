module _ where
open import nat
open import eq
open import bool
open import list
open import e

data EC : SigDepth ->
    (td_whole : TrapDepth) -> (t_whole : T) -> 
    (td_hole : TrapDepth) -> (t_hole : T) -> Set where
  holeEC : ∀ { sd td t } -> EC sd td t td t
  suspEC : ∀ { sd td t n } -> n ∈ sd -> EC (SD sd) td t td t -> EC (SD sd) td t td t
  seqEC : ∀ { sd td t_whole t_hole } -> 
    E sd td t_whole ->
    EC sd td t_whole td t_hole ->
    EC sd td t_whole td t_hole
  parlEC : ∀ { sd td t_hole t } ->
    E sd td t ->
    EC sd td Void td t_hole ->
    EC sd td Void td t_hole

data Decomp : ∀ { sd td_whole T_whole td_hole T_hole } -> 
  E sd td_whole T_whole -> 
  EC sd td_whole T_whole td_hole T_hole -> 
  E sd td_hole T_hole -> 
  Set where

  holeD : ∀ { sd td T } ->
    (e : E sd td T) ->
    Decomp e holeEC e

  suspD : ∀ { sd td T n } ->
    (EC : EC (SD sd) td T td T)
    (e_whole : E (SD sd) td T) ->
    (e_hole : E (SD sd) td T) ->
    (n∈sd : n ∈ sd) ->
    Decomp e_whole EC e_hole ->
    Decomp (e_whole ⊃ n∈sd) (suspEC n∈sd EC) e_hole

  seqD : ∀ { sd td T1 T2 } ->
    (EC : EC sd td T2 td T1) ->
    (e_whole : E sd td T2) ->
    (e_hole : E sd td T1) -> 
    (e_cont : E sd td T2) ->
    Decomp e_whole EC e_hole ->
    Decomp (seq e_whole e_cont) (seqEC e_cont EC) e_hole

  parlD : ∀ { sd td T1 T2 } ->
    (EC : EC sd td T2 td T1) ->
    (e_whole : E sd td T2) ->
    (e_hole : E sd td T1) -> 
    (e_cont : E sd td T2) ->
    Decomp e_whole EC e_hole ->
    Decomp (seq e_whole e_cont) (seqEC e_cont EC) e_hole
