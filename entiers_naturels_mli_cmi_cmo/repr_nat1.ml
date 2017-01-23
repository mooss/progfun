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

let rec est_pair a =
  if est_zero a then true
  else if est_zero(pre a) then false
  else est_pair(pre(pre(a)) );;

est_pair(naturel_of_int 65);;
est_pair(naturel_of_int 12459);;
est_pair(naturel_of_int 48);;

let rec supegal(a, b) =
  if est_zero b then true
  else if est_zero a then false
  else supegal(pre a, pre b);;

supegal(naturel_of_int 752, naturel_of_int 12);;
supegal(naturel_of_int 752, naturel_of_int 753);;
supegal(naturel_of_int 0, naturel_of_int 45424);;

let rec moins = function a, b ->
  if est_zero b then a
  else moins(pre a, pre b);;

int_of_naturel( moins( naturel_of_int 8, naturel_of_int 8));;
int_of_naturel( moins( naturel_of_int 5, naturel_of_int 2));;
int_of_naturel( moins( naturel_of_int 1254, naturel_of_int 1253));;
int_of_naturel( moins( naturel_of_int 23, naturel_of_int 4));;


let rec quotient_et_reste = function a, b ->
  if not(supegal(a, b)) then (zero(), a)
  else let (quo, res) = quotient_et_reste( moins(a, b), b) in
       (plus(quo, suc(zero())), res);;
let map_duo(f, (a, b)) =
  (f a, f b);;

map_duo(int_of_naturel, quotient_et_reste( naturel_of_int 44 , naturel_of_int 5));;
map_duo(int_of_naturel, quotient_et_reste( naturel_of_int 45 , naturel_of_int 5));;
map_duo(int_of_naturel, quotient_et_reste( naturel_of_int 46 , naturel_of_int 5));;

let rec est_puissance_de_deux(nat) =
  if est_zero nat then false
  else if est_zero( pre nat) then true
  else let (quo, res) = quotient_et_reste( nat, suc(suc(zero()))) in
       if not(est_zero res) then false
       else est_puissance_de_deux quo;;

est_puissance_de_deux( naturel_of_int(2047) );;
est_puissance_de_deux( naturel_of_int(2048) );;
est_puissance_de_deux( naturel_of_int(2049) );;

let rec log_base_deux(nat) = 
  if est_zero nat || est_zero(pre nat) then zero()
  else let (quo, res) = quotient_et_reste(nat, suc(suc(zero()))) in
       suc( log_base_deux quo);;

int_of_naturel( log_base_deux( naturel_of_int 1023));;
int_of_naturel( log_base_deux( naturel_of_int 1024));;
int_of_naturel( log_base_deux( naturel_of_int 1025));;
int_of_naturel( log_base_deux( naturel_of_int 2047));;
int_of_naturel( log_base_deux( naturel_of_int 4096));;
