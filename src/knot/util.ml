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

let k = [X(1, 2, 4, 3); P(1, 2); P(3, 4)];;
let k31 = [X(1, 4, 2, 5); X(4, 1, 3, 6); X(6, 3, 5, 2)];;
let k41 = [X(4, 2, 5, 1); X(8, 6, 1, 5); X(6, 3, 7, 4); X(2, 7, 3, 8)];;
let k62 = [X(7,12,8,1); X(1,4,2,5); X(9,3,10,2); X(3,9,4,8); X(5,10,6,11); X(11,6,12,7)];;
let link = [X(1, 6, 2, 4); X(5, 1, 4, 3); P(2,3); P(5,6)];;

(* Utilities *******************************************************************)

let print s=
  print_string s;
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

(* Functions *****************************************************************)

let rec crossing_number (k : pd) : int =
  match k with
  | [] -> 0
  | X(_,_,_,_)::pd -> 1 + crossing_number pd
  | P(_,_)::pd -> crossing_number pd
;;

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

  let max_cut pd = List.fold_left (fun x1 x2 -> if cut x1 >= cut x2 then x1 else x2) (List.hd pd) pd in

  while not (!pd = []) do
    let x = max_cut !pd in
    let () = match x with
    | X(a,b,c,d) -> begin strands.(a) <- true; strands.(b) <- true; strands.(c) <- true; strands.(d) <- true; end
    | _ -> failwith "unreachable"
    in
    pd := List.filter (fun y -> y <> x) !pd;
    out := x::!out;
  done;
  List.rev !out
;;
