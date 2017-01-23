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

let rec plus_rec_un = function a ->
  if est_zero a then un()
  else
    if est_un a then sucsuc(zero())
    else sucsuc( plus_rec_un(prepre(a)) );;

let rec plus = function a, b ->
  if est_zero b then a
  else 
    if est_un b then plus_rec_un a
    else sucsuc( plus(a, (prepre (b))) );;

int_of_naturel( plus( naturel_of_int(15), naturel_of_int(15)));;

let rec moins_rec_un = function a ->
  if est_un a then zero() else
    if(est_zero( prepre(a))) then un() else
      sucsuc( moins_rec_un( prepre(a)));;

let rec moins = function a, b ->
  if est_zero b then a else
    if est_un b then moins_rec_un a else
      moins( prepre(a), prepre(b));;

int_of_naturel( moins( naturel_of_int(15), naturel_of_int(15)));;
int_of_naturel( moins( naturel_of_int(15), naturel_of_int(14)));;
int_of_naturel( moins( naturel_of_int(42), naturel_of_int(23)));;

let rec fois_rec = function a, b ->
  if est_zero b then zero() else
    if est_un b then a else
      plus( plus(a, fois_rec(a, prepre(b))), a);;

let fois = function a, b ->
  if est_zero b || est_zero a then zero() else
    fois_rec(a, b);;

int_of_naturel( fois( naturel_of_int(8), naturel_of_int(4)));;
int_of_naturel( fois( naturel_of_int(8), naturel_of_int(5)));;

let rec supegal = function a, b ->
  if est_zero a then est_zero b
  else if est_un a then est_zero b || est_un b
  else est_zero b || est_un b || supegal(prepre a, prepre b);;


supegal( naturel_of_int(8), naturel_of_int(5));;
supegal( naturel_of_int(2), naturel_of_int(5));;
supegal( naturel_of_int(5), naturel_of_int(5));;

let rec est_pair = function nat ->
  if est_zero nat then true 
  else if est_un nat then false
  else est_pair(prepre nat);;

est_pair( naturel_of_int(5));;
est_pair( naturel_of_int(7));;
est_pair( naturel_of_int(628));;

let rec quotient_et_reste = function x, y ->
  if not (supegal(x, y)) then (zero(), x)
  else let (quo, res) = quotient_et_reste( moins(x, y), y) in
       (plus(quo, un()), res);;

let map_duo = function f, (x, y) ->
  (f x, f y);;

map_duo(int_of_naturel, quotient_et_reste( naturel_of_int(15), naturel_of_int(5)));;
map_duo(int_of_naturel, quotient_et_reste( naturel_of_int(16), naturel_of_int(5)));;

let rec est_puissance_de_deux nat =
  if est_un nat then true
  else let (quo, res) = quotient_et_reste(nat, sucsuc(zero())) in
       if( res != zero() ) then false
       else est_puissance_de_deux quo;;

est_puissance_de_deux( naturel_of_int(3) );;
est_puissance_de_deux( naturel_of_int(1023) );;
est_puissance_de_deux( naturel_of_int(2049) );;
est_puissance_de_deux( naturel_of_int(64) );;
est_puissance_de_deux( naturel_of_int(1024) );;

let rec log_base_deux nat = 
  if est_un nat then zero()
  else plus(un(), (log_base_deux( fst( quotient_et_reste(nat, sucsuc(zero()) ) ))));;

int_of_naturel( log_base_deux( naturel_of_int(8)));;
int_of_naturel( log_base_deux( naturel_of_int(1023)));;
int_of_naturel( log_base_deux( naturel_of_int(1024)));;
int_of_naturel( log_base_deux( naturel_of_int(1025)));;
int_of_naturel( log_base_deux( naturel_of_int(7)));;

