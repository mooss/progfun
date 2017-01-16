#load "nat_1.cmo";;
open Nat_1;;
#install_printer affiche_naturel;;

let rec naturel_of_int = function ent ->
  if ent == 0 then zero()
  else suc(naturel_of_int(ent - 1));;

naturel_of_int 12;;

let rec int_of_naturel = function nat ->
  if est_zero nat then 0
  else 1 + int_of_naturel(pre(nat));;

let rec plus = function a, b ->
  if est_zero b then a
  else plus(suc(a), pre(b));;

let rec fois = function a, b ->
  if est_zero(b) then zero() 
  else plus(a, fois(a,pre(b)))

