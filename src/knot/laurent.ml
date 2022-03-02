type 'a t = {
  mutable coeffs : 'a list;
  mutable least : 'a;
  };;

let zero ={coeffs = [0]; least = 0};;
let one = {coeffs = [1]; least = 0};;

(* ============ fonctions internes ============*)

let rec fill_right n l = match n with
  | 0  -> l
  | m -> fill_right (m-1) (l @ [0])
;;

let rec fill_left n l = match n with
  | 0 -> l
  | m -> fill_left (m-1) (0::l)
;;

let laurent_copy p = {coeffs = p.coeffs; least = p.least};;

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

let shift n p = {coeffs = p.coeffs; least = p.least + n};;

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

let sum p q =
  let m,n = equalize p q in
  if List.length m.coeffs <> List.length n.coeffs then
    begin
      (* Hopefully completely useless now *)
      print_string "Uh Oh! ";
      print_newline ();
      let m_s = m |> string_of_laurent  in
      let n_s = n |> string_of_laurent  in
      let p_s =  p |> string_of_laurent  in
      let q_s =  q |> string_of_laurent  in
      Printf.printf " P = %s " p_s;
      print_newline ();
      Printf.printf " Q = %s " q_s;
      print_newline ();
      Printf.printf " M = %s " m_s;
      print_newline ();
      Printf.printf " N = %s " n_s;
      print_newline ();
    end;
  {coeffs = (List.map2 (+) m.coeffs n.coeffs); least = m.least}
;;

let factor alpha p =
  {coeffs = List.map (fun x -> alpha * x) p.coeffs; least = p.least}
;;

(* Ne permet que des changements de varialble type P(X^k), sans doute pas opti *)
(* TODO: permettre un changement de variable type P(Q(X)) *)
let change_var k p = 
  let i = ref(p.least -1) in
  List.fold_right (fun coef pol -> incr i; sum {coeffs = [coef]; least = !i*k} pol) p.coeffs zero
;;

(* ============ Jones ============ *)
