(* **************************************** *)
(* **************************************** *)

let ab1 = embranche_bin (1, embranche_bin (2, arbin_vide (), arbin_vide ()), embranche_bin (5, arbin_vide (), arbin_vide ())) ;;
let ab2 = embranche_bin (7, embranche_bin (0, arbin_vide (), arbin_vide ()), embranche_bin (8, arbin_vide (), arbin_vide ())) ;;
let ab3 = embranche_bin (4, embranche_bin (11, arbin_vide (), arbin_vide ()), arbin_vide ()) ;;
let ab4 = embranche_bin (6, embranche_bin (10, arbin_vide (), ab3), embranche_bin (9, ab2, embranche_bin (3, ab3, ab1))) ;;

(* **************************************** *)
(* **************************************** *)
