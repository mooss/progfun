#load "arbin.cmo";;
open Arbin;;

(*
nb_noeuds : 'a arbin -> int
nb_feuilles : 'a arbin -> int
est_membre_arbin : 'a * 'a arbin -> bool (TRUE si elt donné est membre de l'arbre)
hauteur_arbin : 'a arbin -> int (cf CM)
arbin_des_doubles : int arbin -> int arbin (double le contenu de chaque noeud)
arbin_des_hauteurs : 'a arbin arbin -> int arbin (donne l'arbre contenant les hauteurs des arbres de chaque noeud correspondant de l'arbre de départ ; droit d'utiliser hauteur_arbin)
rassemble_arbin : 'a arbin * 'b arbin -> ('a * 'b) arbin (arbre de même forme ! donne l'arbre des couples des valeurs des deux arbres)
separe_arbin : ('a * 'b) arbin -> 'a arbin * 'b arbin (inverse de rassemble_arbin)
*)

let ab1 = embranche_bin (1, embranche_bin (2, arbin_vide (), arbin_vide ()), embranche_bin (5, arbin_vide (), arbin_vide ())) ;;
let ab2 = embranche_bin (7, embranche_bin (0, arbin_vide (), arbin_vide ()), embranche_bin (8, arbin_vide (), arbin_vide ())) ;;
let ab3 = embranche_bin (4, embranche_bin (11, arbin_vide (), arbin_vide ()), arbin_vide ()) ;;
let ab4 = embranche_bin (6, embranche_bin (10, arbin_vide (), ab3), embranche_bin (9, ab2, embranche_bin (3, ab3, ab1))) ;;

let rec nb_noeuds arb = 
	if est_arbin_vide(arb)
		then 0
	else
		1 + nb_noeuds(sag(arb)) + nb_noeuds(sad(arb))
;;



let rec nb_feuilles arb = 
	if est_arbin_vide(sag(arb))
		then if est_arbin_vide(sad(arb))
			then 1
		else
			nb_feuilles(sad(arb))
	else
		if est_arbin_vide(sad(arb))
			then nb_feuilles(sag(arb))
		else
			nb_feuilles(sag(arb)) + nb_feuilles(sad(arb))
;;

nb_feuilles(ab4);;

let est_feuille arb =
  est_arbin_vide(sad arb) and est_arbin_vide(sag arb);;

let rec est_membre_arbin(a, arb) =
  if est_arbin_vide arb then false
  else if rac arb = a then true
       else est_membre_arbin(a, sad arb) || est_membre_arbin(a, sag arb);;

est_membre_arbin(5, ab4);;
