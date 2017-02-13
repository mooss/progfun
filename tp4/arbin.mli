type 'a arbin = V | E of 'a * 'a arbin * 'a arbin
exception Arbre_vide
val arbin_vide : unit -> 'a arbin
val embranche_bin : 'a * 'a arbin * 'a arbin -> 'a arbin
val est_arbin_vide : 'a arbin -> bool
val rac : 'a arbin -> 'a
val sag : 'a arbin -> 'a arbin
val sad : 'a arbin -> 'a arbin
