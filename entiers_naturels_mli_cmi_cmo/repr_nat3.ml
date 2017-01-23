#load "nat_3.cmo";;
open Nat_3;;
#install_printer affiche_naturel;;

let rec naturel_of_int = function a ->
  if a = 0 then zero()
  else if a % 2 = 0 then d0(naturel_of_int(a / 2))
  else d1(naturel_of_int(a/2));;

let rec int_of_naturel = function nat ->
  if not(est_pair_et_non_nul nat) then 0
  else
