(** Polynômes de Laurent : Z[X, X^-1] *)
type t = {
  mutable coeffs : int list;
  mutable least : int;
  };;

let zero ={coeffs = [0]; least = 0};;
let one = {coeffs = [1]; least = 0};;

(* ============ fonctions internes ============*)

(** Comble par des zéros à droite  *)
let rec fill_right n l = match n with
  | 0  -> l
  | m -> fill_right (m-1) (l @ [0])
;;

(** Comble par des zéros à gauche *)
let rec fill_left n l = match n with
  | 0 -> l
  | m -> fill_left (m-1) (0::l)
;;

let laurent_copy p = {coeffs = p.coeffs; least = p.least};;

(** Renvoie deux polynômes égaux à p et q dont la liste des coeffs est de même taille, et dont la plus petite puissance présente est égale *)
let equalize p q =

  let m,n = if p.least >= q.least then laurent_copy p, laurent_copy q else laurent_copy q, laurent_copy p in
  let delta = m.least - n.least in
  begin
    m.least <- n.least;
    m.coeffs <- fill_left delta m.coeffs;
  end;

  let m', n' = if List.length m.coeffs >= List.length n.coeffs then m,n else n,m in
  let delta = List.length m'.coeffs - List.length n'.coeffs in
  n'.coeffs <- fill_right delta n'.coeffs;

  (m',n')
;;

(* ============ fonctions utiles ============*)

(** Produit par X^n *)
let shift n p = {coeffs = p.coeffs; least = p.least + n};;

let sum p q =
  let m,n = equalize p q in
 {coeffs = (List.map2 (+) m.coeffs n.coeffs); least = m.least}
;;

let factor alpha p =
  {coeffs = List.map (fun x -> (alpha * x)) p.coeffs; least = p.least}
;;

let string_of_laurent p =
  let rec string_of_int_list = function
  | [] -> ""
  | t::q -> string_of_int t ^ "; " ^ string_of_int_list q
  in
  Printf.sprintf " { coeffs = [ %s ] ; least = %i }" (string_of_int_list p.coeffs) p.least
;;

let string_of_laurent_pretty p =
  let k = ref(p.least) in
  let out = ref("") in
  let f coef = 
    begin
    match coef with
      | 1 -> out := (Printf.sprintf "+ A^%i %s" !k !out);
      | 0 -> ();
      | -1 -> out := (Printf.sprintf "- A^%i %s" !k !out)
      | n when n < 0 -> out := Printf.sprintf "- %iA^%i %s" (abs coef) !k !out;
      | _ -> out := Printf.sprintf "+ %iA^%i %s" coef !k !out;
    end;
    incr k;
  in
  List.iter f p.coeffs;
  !out
;;
