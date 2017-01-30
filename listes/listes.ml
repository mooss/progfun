open List;;

let lost = [4; 8; 15; 16; 23; 42];;

let rec ajout_en_fin(el, lst) =
  if lst = [] then [el]
    else hd lst :: ajout_en_fin(el, tl lst);;

ajout_en_fin( 42, [4; 8; 15; 16; 23]);;

let rec inverse lst =
  if lst = [] then []
  else ajout_en_fin(hd lst, inverse(tl lst));;

inverse([4; 8; 15; 16; 23; 42]);;

let rec dernier lst =
  if tl lst = [] then hd lst
  else dernier(tl lst);;

dernier(lost);;

let rec sans_dernier lst =
  if tl lst = [] then []
  else hd lst :: sans_dernier(tl lst);;

sans_dernier(lost);;
sans_dernier(inverse(lost));;

let rec dernier_sans_dernier lst =
  if tl lst = [] then (hd lst, [])
  else let (der, sader) = dernier_sans_dernier()
