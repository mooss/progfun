#load "nat_3.cmo";;
open Nat_3;;
#install_printer affiche_naturel;;

let rec naturel_of_int = function a ->
  if a = 0 then zero()
  else if a mod 2 = 0 then d0(naturel_of_int(a / 2))
  else d1(naturel_of_int(a/2));;

(* let est_nul = function nat -> *)
(*   (not est_pair_et_non_nul(nat)) && not est_impair(nat);; *)

let rec int_of_naturel = function nat ->
  if not(est_pair_et_non_nul(nat)) && not( est_impair(nat)) then 0
  else if est_impair nat then 2 * ( int_of_naturel(inv_d1 nat)) + 1
  else 2 * int_of_naturel(inv_d0 nat);;

int_of_naturel( naturel_of_int( 55));;

naturel_of_int(4);;

let est_pair a =
  not(est_impair a);;

est_pair(naturel_of_int(412));;
est_pair(naturel_of_int(15));;
est_pair(naturel_of_int(0));;

(* let fois(a, b) =  *)

inv_d1( naturel_of_int(1));;
