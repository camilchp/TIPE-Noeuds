type t = {
  num : int;
  den : int
};;

let rec gcd a b = 
  let m, n = if a >= b then a,b else b,a in
  match m mod n with
  | 0 -> n
  | r -> gcd n r
;;

let reduce q =
  let d = gcd q.num q.den in
  {num = q.num / d; den = q.den / d}
;;
  
let string_of_q q =
  let qr = reduce q in
  if qr.den = 1 then string_of_int qr.num
  else Printf.sprintf "(%i/%i)" qr.num qr.den
;;