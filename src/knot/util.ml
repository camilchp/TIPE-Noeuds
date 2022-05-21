open Laurent;;

(* Types ***********************************************************************)

(** Each number corresponds to a strand : X represents a crossing, P a point joining two strands together.*)
type node =
  | X of int * int * int * int                  (* c   b  *)
  | P of int * int                              (*  \ /   *)
                                                (*   /    *)
                                                (*  / \   *)
                                                (* d   a  *)
;;

type pd = node list;;

(* Global Variables ***********************************************************)

let loop = [X(4,2,1,1); X(2,4,3,3)];;
let k31 = [X(1, 4, 2, 5); X(4, 1, 3, 6); X(6, 3, 5, 2)];;
let k41 = [X(4, 2, 5, 1); X(8, 6, 1, 5); X(6, 3, 7, 4); X(2, 7, 3, 8)];;
let k62 = [X(7,12,8,1); X(1,4,2,5); X(9,3,10,2); X(3,9,4,8); X(5,10,6,11); X(11,6,12,7)];;
let link = [X(1, 6, 2, 4); X(5, 1, 4, 3); P(2,3); P(5,6)];;
let gst48 = [X(1, 15, 2, 14); X(29, 2, 30, 3); X(40, 4, 41, 3); X(4, 44, 5, 43); X(5, 26, 6, 27);X(95, 7, 96, 6); X(7, 1, 8, 96); X(8, 14, 9, 13); X(28, 9, 29, 10); X(41, 11, 42, 10);X(11, 43, 12, 42); X(12, 27, 13, 28); X(15, 31, 16, 30); X(61, 16, 62, 17); X(72, 17, 73, 18);X(83, 18, 84, 19); X(34, 20, 35, 19); X(20, 89, 21, 90); X(92, 21, 93, 22); X(22, 79, 23, 80);X(23, 68, 24, 69); X(24, 57, 25, 58); X(56, 25, 57, 26); X(31, 63, 32, 62); X(32, 74, 33, 73);X(33, 85, 34, 84); X(35, 50, 36, 51); X(81, 37, 82, 36); X(70, 38, 71, 37); X(59, 39, 60, 38);X(54, 39, 55, 40); X(55, 45, 56, 44); X(45, 59, 46, 58); X(46, 70, 47, 69);X(47, 81, 48, 80); X(91, 49, 92, 48); X(49, 91, 50, 90); X(82, 52, 83, 51); X(71, 53, 72, 52);X(60, 54, 61, 53); X(74, 63, 75, 64); X(85, 64, 86, 65); X(65, 76, 66, 77);X(66, 87, 67, 88); X(94, 67, 95, 68); X(86, 75, 87, 76); X(77, 88, 78, 89); X(93, 78, 94, 79)];;
let thistlewaite_unknot = [X(14,24,15,23); X(10,16,11,15); X(29,13,30,12); X(25,17,26,16); X(26,3,27,4); X(2,18,3,17); X(24,9,25,10); X(1,8,2,9); X(18,28,19,27); X(13,1,14,30); X(4,21,5,22); X(22,12,23,11); X(20,5,21,6); X(6,19,7,20); X(7,28,8,29)]

(* Utilities *******************************************************************)

let print s=
  print_endline s;
  flush stdout;
;;

let node_map f = function
  | X (a,b,c,d) -> X (f a, f b, f c, f d)
  | P (a,b) -> P (f a, f b)
;;

let pd_map f k = List.map (node_map f) k;;

let string_of_node = function
  | X (a,b,c,d) -> Printf.sprintf "X(%i,%i,%i,%i)" a b c d
  | P (a, b) -> Printf.sprintf "P(%i,%i)" a b
;;

let string_of_pd k = "[" ^ (k |> List.map string_of_node |> String.concat " ") ^ "]";;

let string_of_pol (f : pd -> Laurent.t) (k : pd) : string = k |> f |> string_of_laurent_pretty;;

(** sorts a list in O(n log(n)) according to given order *)
let rec sort ord l =

  let rec merge ord l1 l2 =
  match l1,l2 with
  | _,[] -> l1
  | [],_ -> l2
  | t1::q1, t2::q2 -> if ord t1 t2 then t1::(merge ord q1 l2) else t2::(merge ord l1 q2)
  in

  let rec split = function
  | [] -> [],[]
  | [a] -> [a],[]
  | a::b::q -> let l1,l2 = split q in a::l1, b::l2
  in

  match l with
  | [] -> []
  | [a] -> [a]
  | _ -> let l1,l2 = split l in merge ord (sort ord l1) (sort ord l2)
;;

(** reduces a list according to given reduction *)
let rec reduce (reduction : 'a -> 'a -> 'a option) (lst : 'a list) =

  (* reduce single element in list *)
  let reduce_1 el l =
    let success = ref false in
    let rec aux = function
      | [] -> []
      | t::q -> match reduction el t with
                | None -> t::(aux q)
                | Some(t') -> success := true; t'::q
    in
    let l' = aux l in
    (!success, l')
  in

  match lst with
  | [] -> []
  | t::q -> match reduce_1 t q with
            |(true, q') -> reduce reduction q'
            |(false, _) -> t::(reduce reduction q)
;;

(* Functions *****************************************************************)

let rec crossing_number (k : pd) : int =
  match k with
  | [] -> 0
  | X(_,_,_,_)::pd -> 1 + crossing_number pd
  | P(_,_)::pd -> crossing_number pd
;;

(** Computes Kauffman Bracket in O(n2^n) time *)
let rec bracket (k : pd) : Laurent.t =
  match k with
  | [P(a,b)] when a = b -> one                                                                                          (* < o > = 1                       *)
  | P(a, b)::pd when a = b -> let p = bracket pd in sum (shift 2 p) (shift (-2) p ) |> factor (-1)                      (* < o L > = (-A^2 - A^-2) <L>     *)
  | P(a,b)::pd -> pd |> pd_map (fun c -> if c = a then b else c) |> bracket                                             (* réduction des jointures         *)
  | X(a,b,c,d)::pd -> sum (P(a,d)::P(b,c)::pd |> bracket |> shift (-1)) (P(a,b)::P(c,d)::pd |> bracket |> shift 1)      (* < X > = A^-1 < = > + A^1 < || > *)
  | _ -> let s = string_of_pd k in Printf.printf "bracket : %s \n" s; failwith "bracket : match failure"
;;

(** Describes direction in oriented knot-pd. Fails in rare case when n+1 --> n (picture a single loop intersecting a line segment) *)
let (-->) a b = (b = (a+1)) || (a <> (b+1) && b < a);; (* TODO: identify and simplify these cases ? seems difficult... *)

let sign node : int =
  match node with
  | X(a,b,c,d) -> if (c --> a && b --> d) || (a --> c && d --> b) then ((*print_endline (Printf.sprintf " +1 -- a : %i, b : %i, c : %i, d : %i" a b c d);*) 1) else ((*print_endline (Printf.sprintf " -1 -- a : %i, b : %i, c : %i, d : %i" a b c d);*) -1)
  | P(_,_) -> 0
;;

let rec writhe (k : pd) : int =
  match k with
  | [] -> 0
  | node::pd -> sign node + writhe pd
;;

let kauffman_x (k : pd) : Laurent.t =
  let w = writhe k in
  k |> bracket |> shift (-3*w) |> (fun p -> if w mod 2 = 0 then p else factor (-1) p )
;;

(* Works only if powers are divisible by four !*)
(* TODO: make it work when only divisible by two : all kauffman_x are ! *)
let jones (k : pd) : Laurent.t =
  let p = kauffman_x k in
  let i = ref(p.least-1) in
  List.fold_left (fun q coeff ->
    incr i;
    if coeff <> 0
      then (sum q {coeffs = [coeff]; least = -(!i/4)})
      else q
  ) zero p.coeffs
;;

(* Fast Kauffman Bracket ******************************************************)

(** Thin position returns a pd of the given knot, in an order that maximises connexity. Starts with the first crossing and finds next best crossing in a greedy fashion*)
let thin_position (k : pd) : pd =
  let strands = Array.make (2 * (List.length k) + 1) false in
  let pd = ref k in
  let out = ref [] in

  let cut x = match x with
  | X(a,b,c,d) -> List.fold_left (fun n i -> if strands.(i) then n+1 else n) 0 [a;b;c;d]
  | _ -> failwith "unreachable"
  in

  let max_cut pd = List.fold_left (fun x1 x2 -> if cut x1 > cut x2 then x1 else x2) (X(0,0,0,0)) pd in

  while not (!pd = []) do
    let x = max_cut !pd in
    let () = match x with
    | X(a,b,c,d) -> begin strands.(a) <- true; strands.(b) <- true; strands.(c) <- true; strands.(d) <- true; end
    | _ -> failwith "unreachable"
    in
    pd := List.filter (fun y -> y <> x) !pd;
    out := x::!out;
  done;
  !out
;;

(** Linear combination of Base-tangles with polynomial coefficients (ex: (A^1 + 1)[P12;P34]) *)
type cl = (Laurent.t * (pd list)) list;;

let cl_map f_pol f_pd = List.map (fun (pol, pd) -> (f_pol pol, f_pd pd));;

let cl_map_pd f = cl_map (fun p -> p) f;;

(** lexicographic order on Pij's *)
let [@warning "-8"] (<<=) (P(a,b)) (P(c,d)) = if a = c then b <= d else a < c;;

(** Choosing an convention for order of coefficients, since Pij = Pji *)
let [@warning "-8"] tweak (P(a,b)) = if a >= b then P(a,b) else P(b,a);;

(** Simplifies list of Pij's with [Pij Pjk] -> [Pik] moves *)
let reduce_base_tangle =
  let [@warning "-8"] joined (P(a,b)) (P(c,d)) =
    if a = c then Some(P(b,d)) else
    if a = d then Some(P(b,c)) else
    if b = c then Some(P(a,d)) else
    if b = d then Some(P(a,c))
    else None
  in
  reduce joined
;;

let simplify_loops cl =
  let reduction (pol, pd) =
    let p = ref pol in
    let only_loops = ref true in
    let rec aux pd =
      match pd with
      | [] -> []
      | [P(i,j)] when i = j && !only_loops -> []
      | P(i,j)::q when i = j ->  p := (factor (-1) (sum (shift (-2) !p) (shift 2 !p))); aux q
      | t::q -> only_loops := false; t::(aux q)
    in
    let pd' = aux pd in
    (!p, pd')
  in
  List.map reduction cl
;;

(* TODO: map normalisation once at the start ! *)
(** Simplifies linear combination by checking for equal Base-tangles *)
let simplify_cl =
  let reduction (pol1, pd1) (pol2, pd2) =
    let pd1' = pd1 |> List.map tweak |> sort (<<=) and pd2' = pd2 |> List.map tweak |> sort (<<=) in
    if pd1' = pd2'
    then Some((sum pol1 pol2), pd1')
    else None
  in
  reduce reduction
;;

let string_of_cl cl =
  let s = cl |> List.map (fun (pol,pd) -> Printf.sprintf "(%s , %s);\n" (string_of_laurent_pretty pol) (string_of_pd pd)) |> List.fold_left (^) "" in
  Printf.sprintf "[ %s ]\n" s
;;

(** Computes Kauffman bracket in O(pol(n)2^sqrt(n)) time *)
let fast_bracket k =

  let k' = thin_position k in
  let cl = ref [ (one,[]) ] in

  let [@warning "-8"] add_crossing (X(a,b,c,d)) =
    (*  print (string_of_cl !cl); *)
    let cl1 = cl_map (shift (-1)) (fun pd -> P(a,d)::P(b,c)::pd) !cl in
    let cl2 = cl_map (shift 1) (fun pd -> P(a,b)::P(c,d)::pd) !cl in
    cl := cl1 @ cl2
          |> cl_map_pd reduce_base_tangle
          |> simplify_loops
          |> simplify_cl
  in

  List.iter add_crossing k';

  match !cl with
  | [(pol,[])] -> pol
  | _ -> failwith "fast_bracket : could not simplify to tangle-less polynomial"
;;

let fast_kauffman_x (k : pd) : Laurent.t =
  let w = writhe k in
  k |> fast_bracket |> shift (-3*w) |> (fun p -> if w mod 2 = 0 then p else factor (-1) p )
;;

(* Works only if powers are divisible by four !*)
let fast_jones (k : pd) : Laurent.t =
  let p = fast_kauffman_x k in
  let i = ref(p.least-1) in
  List.fold_left (fun q coeff ->
    incr i;
    if coeff <> 0
      then (sum q {coeffs = [coeff]; least = -(!i/4)})
      else q
  ) zero p.coeffs
;;
