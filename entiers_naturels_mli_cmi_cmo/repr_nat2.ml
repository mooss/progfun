#load "nat_2.cmo";;
open Nat_2;;
#install_printer affiche_naturel;;

let rec naturel_of_int = function (ent) ->
  if ent == 0 then zero()
  else if ent == 1 then un()
  else sucsuc(naturel_of_int(ent-2));;

let rec int_of_naturel = function (nat) ->
  if est_zero(nat) then 0
  else if est_un(nat) then 1
  else 2 + int_of_naturel(prepre(nat));;

let rec plus = function a b ->
  if est_zero b
    then plus_rec_zero a
  else if est_un a
      then plus_rec_un a
